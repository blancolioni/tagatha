with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Conversions;

package body Tagatha.Arch.M6502 is

   function Hex_Image (X : Word_64) return String;

   function To_6502_Label (Name : String) return String;

   function Local_Label (L : Positive) return String;

   function Indexed_Label
     (Routine_Name : String;
      Index        : Positive)
      return String
   is (To_6502_Label (Routine_Name)
       & "__"
       & (if Index >= 10
         then [Character'Val (Index / 10 + 48)]
         else [])
       & Character'Val (Index mod 10 + 48));

   function Argument_Label
     (Routine_Name : String;
      Index        : Argument_Index)
      return String
   is (Indexed_Label (Routine_Name & "__arg", Positive (Index)));

   function Local_Label
     (Routine_Name : String;
      Index        : Local_Index)
      return String
   is (Indexed_Label (Routine_Name & "__loc", Positive (Index)));

   type No_Operand_Instance is new M6502_Operand_Instance with null record;

   overriding function Image (This : No_Operand_Instance) return String
   is ("");

   type Argument_Operand_Instance is new M6502_Operand_Instance with
      record
         Index : Argument_Index;
         Label : Tagatha.Names.Tagatha_Name;
      end record;

   overriding function Image (This : Argument_Operand_Instance) return String;

   type Local_Operand_Instance is new M6502_Operand_Instance with
      record
         Index : Local_Index;
         Label : Tagatha.Names.Tagatha_Name;
      end record;

   overriding function Image (This : Local_Operand_Instance) return String;

   overriding function Offset_Image
     (This        : Local_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String;

   type Result_Operand_Instance is new M6502_Operand_Instance with
      record
         Index : Result_Index;
      end record;

   overriding function Is_Accumulator
     (This : Result_Operand_Instance)
      return Boolean
   is (True);

   overriding function Image (This : Result_Operand_Instance) return String;

   type Return_Operand_Instance is new M6502_Operand_Instance with
      record
         Index : Return_Index;
      end record;

   overriding function Is_Accumulator
     (This : Return_Operand_Instance)
      return Boolean
   is (True);

   overriding function Image (This : Return_Operand_Instance) return String;

   type Temporary_Operand_Instance is new M6502_Operand_Instance with
      record
         Index       : Temporary_Index;
         R           : Temporary_Register_Index;
         First_Write : Boolean;
         Last_Read   : Boolean;
      end record;

   overriding function Image
     (This : Temporary_Operand_Instance)
      return String;

   type Constant_Operand_Instance is new M6502_Operand_Instance with
      record
         Value    : Word_64;
      end record;

   overriding function Image (This : Constant_Operand_Instance) return String;
   overriding function Word_Size
     (This : Constant_Operand_Instance)
      return Positive
   is (case This.Content is
          when General_Content => 1,
          when Floating_Point_Content => 2);

   overriding function Offset_Image
     (This        : Constant_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String
   is ('#' & Hex_Image (This.Value / 2 ** (Word_Offset * 16) mod 65536));

   overriding function Is_Constant
     (This : Constant_Operand_Instance;
      Value : Integer)
      return Boolean
   is (This.Content = General_Content
       and then Conversions.Int_32_To_Word_64 (Int_32 (Value))
       = This.Value);

   type External_Operand_Instance is new M6502_Operand_Instance with
      record
         Name    : Tagatha.Names.Tagatha_Name;
         Address : Boolean;
      end record;

   overriding function Image (This : External_Operand_Instance) return String;

   overriding function No_Operand
     (This  : Instance)
      return Operand_Interface'Class
   is (No_Operand_Instance'(Content => General_Content));

   overriding function Argument_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Argument_Index)
      return Operand_Interface'Class
   is (Argument_Operand_Instance'(Content => Content,
                                  Index   => Index,
                                  Label   => Tagatha.Names.To_Name
                                    (Argument_Label
                                       (Tagatha.Names.To_String (This.Routine),
                                        Index))));

   overriding function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Local_Index)
      return Operand_Interface'Class
   is (Local_Operand_Instance'(Content => Content,
                               Index   => Index,
                               Label   => Tagatha.Names.To_Name
                                 (Local_Label
                                    (Tagatha.Names.To_String (This.Routine),
                                     Index))));

   overriding function Result_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Result_Index)
      return Operand_Interface'Class
   is (Result_Operand_Instance'(Content => Content,
                                Index   => Index));

   overriding function Return_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Return_Index)
      return Operand_Interface'Class
   is (Return_Operand_Instance'(Content => Content,
                                Index   => Index));

   overriding function Name_Operand
     (This    : Instance;
      Name    : String;
      Address  : Boolean;
      Imported : Boolean)
      return Operand_Interface'Class
   is (External_Operand_Instance'(Content => General_Content,
                                  Name    => Tagatha.Names.To_Name (Name),
                                  Address => Address));

   ---------
   -- ADC --
   ---------

   procedure ADC
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class)
   is
      Op : M6502_Operand_Instance'Class renames
             M6502_Operand_Instance'Class (Operand);
   begin
      if not Op.Is_Zero then
         This.Put_Instruction ("ADC", Op.Image);
         This.A := Tagatha.Names.Empty_Name;
      end if;
   end ADC;

   -------------------
   -- Begin_Routine --
   -------------------

   overriding procedure Begin_Routine
     (This      : in out Instance;
      Name      : String;
      Arguments : Argument_Count;
      Results   : Result_Count;
      Locals    : Local_Count;
      Linkage   : Boolean)
   is
      Label : constant String :=
                To_6502_Label (Name);
   begin

      This.Routine := Tagatha.Names.To_Name (Name);

      if This.Option (No_Recursion) then
         for I in 1 .. Arguments loop
            This.Put_Line
              (".var "
               & Argument_Label (Name, I)
               & " = "
               & Hex_Image (This.Next_ZP));
            This.Next_ZP := @ + 1;
         end loop;

         for I in 1 .. Locals loop
            This.Put_Line
              (".var "
               & Local_Label (Name, I)
               & " = "
               & Hex_Image (This.Next_ZP));
            This.Next_ZP := @ + 1;
         end loop;
      end if;

      This.Put_Line (Label & ":");

      if This.Options (No_Recursion) then
         if Arguments >= 1 then
            This.Put_Instruction ("sta", Argument_Label (Name, 1));
         end if;
         if Arguments >= 2 then
            This.Put_Instruction ("stx", Argument_Label (Name, 2));
         end if;
         if Arguments >= 3 then
            This.Put_Instruction ("sty", Argument_Label (Name, 3));
         end if;

      else
         This.Put_Instruction ("lda", "FP");
         This.Put_Instruction ("pha");
         This.Put_Instruction ("tsx");
         This.Put_Instruction ("stx", "FP");
         for I in 1 .. Locals loop
            This.Put_Instruction ("dex");
         end loop;
      end if;
   end Begin_Routine;

   ------------
   -- Branch --
   ------------

   overriding procedure Branch
     (This        : in out Instance;
      Operand     : Operand_Interface'Class;
      Condition   : Branch_Condition;
      Destination : Positive;
      Forward     : Boolean)
   is
   begin
      if Condition = Always then
         This.Put_Instruction ("jmp", Local_Label (Destination));
      else
         This.LDA (Operand);
         This.Put_Instruction
           ((if Condition = Z then "beq" else "bne"),
            Local_Label (Destination));
      end if;
   end Branch;

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (This           : in out Instance;
      Name           : Operand_Interface'class;
      Actuals        : Operand_Lists.List;
      Result_Count   : Natural)
   is
      Arg_Reg : constant String := "AXY";
      Arg_Index : Natural := Natural (Actuals.Length);
   begin
      for Arg of reverse Actuals loop
         if Arg_Index = 1 then
            This.LDA (Arg);
         elsif Arg_Index <= Arg_Reg'Last then
            This.Put_Instruction ("LD" & Arg_Reg (Arg_Index), Arg.Image);
         else
            This.LDA (Arg);
            declare
               Arg_Label : constant String :=
                             Argument_Label
                               (Name.Image, Argument_Index (Arg_Index));
            begin
               This.Put_Instruction
                 ("STA", Arg_Label);
               This.A := Tagatha.Names.To_Name (Arg_Label);
            end;
         end if;
         Arg_Index := Arg_Index - 1;
      end loop;

      This.Put_Instruction
        ("jsr", Name.Image);

      This.A := Tagatha.Names.Empty_Name;
      This.X := Tagatha.Names.Empty_Name;
      This.Y := Tagatha.Names.Empty_Name;

   end Call;

   ----------------------
   -- Constant_Operand --
   ----------------------

   overriding function Constant_Operand
     (This     : Instance;
      Content  : Operand_Content;
      Value    : Word_64)
      return Operand_Interface'Class
   is
      Final_Value : Word_64 := Value;
   begin
      case Content is
         when Floating_Point_Content =>
            declare
               function To_Word_32 is
                 new Ada.Unchecked_Conversion (Float, Word_32);
               F_64 : constant Floating_Point_Constant :=
                        Conversions.Word_64_To_Floating_Point (Value);
               F_32 : constant Float := Float (F_64);
               W_32 : constant Word_32 := To_Word_32 (F_32);
            begin
               Final_Value := Word_64 (W_32);
            end;
         when General_Content =>
            null;
      end case;

      return Constant_Operand_Instance'(Content, Final_Value);
   end Constant_Operand;

   -----------------
   -- End_Routine --
   -----------------

   overriding procedure End_Routine
     (This : in out Instance)
   is
   begin
      null;
   end End_Routine;

   ------------------
   -- Exit_Routine --
   ------------------

   overriding procedure Exit_Routine
     (This : in out Instance)
   is
   begin
      This.Put_Instruction ("rts");
   end Exit_Routine;

   ------------------
   -- Fail_Routine --
   ------------------

   overriding procedure Fail_Routine
     (This : in out Instance)
   is
   begin
      This.Put_Instruction ("jsr __tagatha_exception_handler");
   end Fail_Routine;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Argument_Operand_Instance)
      return String
   is
   begin
      return Tagatha.Names.To_String (This.Label);
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Constant_Operand_Instance)
      return String
   is
   begin
      return "#" & Hex_Image (This.Value);
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : External_Operand_Instance)
      return String
   is
   begin
      return To_6502_Label (Tagatha.Names.To_String (This.Name));
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Local_Operand_Instance)
      return String
   is
   begin
      return Tagatha.Names.To_String (This.Label);
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Result_Operand_Instance)
      return String
   is
   begin
      return "A";
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Return_Operand_Instance)
      return String
   is
   begin
      return "A";
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Temporary_Operand_Instance)
      return String
   is
      Prefix : constant String :=
                 (case This.Content is
                     when General_Content => "r",
                     when Floating_Point_Content => "AC");
      Index  : constant Character := Character'Val (48 + Natural (This.R));
   begin
      return Prefix & Index;
   end Image;

   ----------
   -- Jump --
   ----------

   overriding procedure Jump
     (This           : in out Instance;
      Name           : Operand_Interface'Class)
   is
   begin
      This.Put_Instruction ("JMP", Name.Image);
   end Jump;

   ---------
   -- LDA --
   ---------

   procedure LDA
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class)
   is
      Op : M6502_Operand_Instance'Class renames
             M6502_Operand_Instance'Class (Operand);
      Current_Op : constant String := Tagatha.Names.To_String (This.A);
      New_Op     : constant String := Operand.Image;
   begin
      if not Op.Is_Accumulator
        and then Current_Op /= New_Op
      then
         This.Put_Instruction ("lda", New_Op);
         This.A := Tagatha.Names.To_Name (New_Op);
      end if;
   end LDA;

   ---------
   -- LDX --
   ---------

   procedure LDX
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class)
   is
      Current_Op : constant String := Tagatha.Names.To_String (This.X);
      New_Op     : constant String := Operand.Image;
   begin
      if Current_Op /= New_Op then
         This.Put_Instruction ("LDX", New_Op);
         This.X := Tagatha.Names.To_Name (New_Op);
      end if;
   end LDX;

   -----------------
   -- Local_Label --
   -----------------

   function Local_Label (L : Positive) return String is
      Img : constant String := L'Image;
   begin
      return "L" & Img (2 .. Img'Last);
   end Local_Label;

   overriding procedure Local_Label
     (This  : in out Instance;
      Label : Positive)
   is
   begin
      Parent (This).Local_Label (Label);
      This.A := Tagatha.Names.Empty_Name;
      This.X := Tagatha.Names.Empty_Name;
      This.Y := Tagatha.Names.Empty_Name;
   end Local_Label;

   ----------------
   -- Name_Label --
   ----------------

   overriding procedure Name_Label
     (This : in out Instance;
      Name : String)
   is
   begin
      This.Put_Line (Name & ":");
      This.A := Tagatha.Names.Empty_Name;
      This.X := Tagatha.Names.Empty_Name;
      This.Y := Tagatha.Names.Empty_Name;
   end Name_Label;

   -----------------
   -- Hex_Image --
   -----------------

   function Hex_Image (X : Word_64) return String is
      It  : Word_64 := X;
      Img : String (1 .. 12) := "000000000000";
      Hex : constant String := "0123456789ABCDEF";
      Last : Natural := Img'Last;
   begin
      if It = 0 then
         return "0";
      end if;

      for Ch of reverse Img loop
         Ch := Hex (Natural (It mod 16) + 1);
         It := It / 16;
         if It = 0 and then Last mod 2 /= 0 then
            return '$' & Img (Last .. Img'Last);
         end if;
         Last := Last - 1;
      end loop;
      return '$' & Img;
   end Hex_Image;

   ------------------
   -- Offset_Image --
   ------------------

   overriding function Offset_Image
     (This        : Local_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String
   is
      Img    : constant String := Word_Offset'Image;
   begin
      return Tagatha.Names.To_String (This.Label)
        & (if Word_Offset = 0 then ""
           else " -" & Img);
   end Offset_Image;

   -----------------
   -- Put_Comment --
   -----------------

   overriding procedure Put_Comment
     (This    : in out Instance;
      Comment : String)
   is
   begin
      This.Lines.Append ("// " & Comment);
   end Put_Comment;

   ---------------------
   -- Put_Data_Buffer --
   ---------------------

   overriding procedure Put_Data_Buffer (This : in out Instance) is
      use Ada.Strings.Unbounded;
      S : Unbounded_String;
      First : Boolean := True;
   begin
      if This.Data_Bits <= 8 then
         S := To_Unbounded_String ("    .byte ");
      else
         S := To_Unbounded_String ("    .word ");
      end if;
      for Value of This.Data_Buffer loop
         if First then
            S := S & Value;
            First := False;
         else
            S := S & "," & Value;
         end if;
      end loop;
      This.Put_Line (To_String (S));
      This.Data_Buffer.Clear;
   end Put_Data_Buffer;

   ---------------------
   -- Raise_Exception --
   ---------------------

   overriding procedure Raise_Exception
     (This    : in out Instance;
      E       : Operand_Interface'Class)
   is
   begin
      This.Put_Instruction ("jsr __tagatha_exception_handler");
   end Raise_Exception;

   -----------
   -- Retry --
   -----------

   overriding procedure Retry
     (This        : in out Instance;
      Destination : String)
   is
   begin
      This.Put_Instruction ("JMP", Destination);
   end Retry;

   ---------
   -- STA --
   ---------

   procedure STA
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class)
   is
      Op         : M6502_Operand_Instance'Class renames
                     M6502_Operand_Instance'Class (Operand);
   begin
      if not Op.Is_Accumulator then
         This.Put_Instruction ("sta", Op.Image);
         This.A := Tagatha.Names.To_Name (Op.Image);
      end if;
   end STA;

   -----------------------
   -- Temporary_Operand --
   -----------------------

   overriding function Temporary_Operand
     (This        : in out Instance;
      Index       : Temporary_Index;
      Content     : Operand_Content;
      First_Write : Boolean;
      Last_Read   : Boolean)
      return Operand_Interface'Class
   is
      R : Register_Index := 0;
   begin

      for T in This.Temps'Range loop
         if First_Write then
            if This.Temps (T) = 0 then
               This.Temps (T) := Index;
               R := T;
               exit;
            end if;
         else
            if This.Temps (T) = Index then
               R := T;
               if Last_Read then
                  This.Temps (T) := 0;
               end if;
               exit;
            end if;
         end if;
      end loop;

      if R = 0 then
         for T in This.Temps'Range loop
            Ada.Text_IO.Put_Line
              ("R" & T'Image & " -> T" & This.Temps (T)'Image);
         end loop;
         raise Constraint_Error with
           "no spare registers for temporary" & Index'Image;
      end if;

      return Temporary_Operand_Instance'
        (Content, Index, R, First_Write, Last_Read);

   end Temporary_Operand;

   -------------------
   -- To_6502_Label --
   -------------------

   function To_6502_Label (Name : String) return String is
   begin
      for I in Name'Range loop
         if Name (I) = '.' then
            return Name (Name'First .. I - 1)
              & "__"
              & To_6502_Label (Name (I + 1 .. Name'Last));
         end if;
      end loop;
      return Name;
   end To_6502_Label;

   --------------
   -- Transfer --
   --------------

   overriding procedure Transfer
     (This         : in out Instance;
      Dst          : Operand_Interface'Class;
      Src_1, Src_2 : Operand_Interface'Class;
      Op           : Operator)
   is
      Op_Name : constant String :=
                  (case Op is
                      when Op_Identity    => "",
                      when Op_Negate      => "NEG",
                      when Op_Not         => "NOT",
                      when Op_Test        => "TST",
                      when Op_Add         => "ADD",
                      when Op_Subtract    => "SUB",
                      when Op_Multiply    => "MUL",
                      when Op_Divide      => "DIV",
                      when Op_Mod         => "DIV",
                      when Op_Fadd        => "ADDF",
                      when Op_Fsub        => "SUBF",
                      when Op_Fmul        => "MULF",
                      when Op_Fdiv        => "DIVF",
                      when Op_And         => "BIC",
                      when Op_Or          => "BIS",
                      when Op_Xor         => "XOR",
                      when Op_Dereference => "MOV",
                      when Op_Store       => "MOV",
                      when Op_EQ          => "bne",
                      when Op_NE          => "beq",
                      when Op_LT          => "bmi",
                      when Op_LE          => "bmi",
                      when Op_GT          => "bpl",
                      when Op_GE          => "bpl");

      Src_1_Op : M6502_Operand_Instance'Class renames
                   M6502_Operand_Instance'Class (Src_1);
      Src_2_Op : M6502_Operand_Instance'Class renames
                   M6502_Operand_Instance'Class (Src_2);
      Dst_Op   : M6502_Operand_Instance'Class renames
                   M6502_Operand_Instance'Class (Dst);

      type Src_Index is range 1 .. 2;
      Src_1_Image : constant String := Src_1.Image;
      Src_2_Image : constant String := Src_2.Image;
      Dst_Image   : constant String := Dst.Image;

      procedure Src_To_Dst (Idx : Src_Index);

      ------------------
      -- Src_2_To_Dst --
      ------------------

      procedure Src_To_Dst (Idx : Src_Index) is
         Src_Image : constant String :=
                       (if Idx = 1 then Src_1_Image else Src_2_Image);
         Src_Op    : constant M6502_Operand_Instance'Class :=
                       (if Idx = 1 then Src_1_Op else Src_2_Op);
      begin
         if Src_Image /= Dst_Image then
            if Src_Op.Content = Floating_Point_Content then
               This.Put_Instruction
                 ("LDF", Src_Image, "AC0");
               This.Put_Instruction
                 ("STF", "AC0", Dst_Image);
            elsif Src_Op.Word_Size = 1
              and then Dst_Op.Word_Size = 1
            then
               This.LDA (Src_Op);
               This.STA (Dst_Op);
            else
               declare
                  Offset : Natural := 0;
               begin
                  for I in 1 .. Natural'Max (Src_Op.Word_Size,
                                             Dst_Op.Word_Size)
                  loop
                     This.Put_Instruction
                       ("MOV",
                        Src_Op.Offset_Image (True, Offset),
                        Dst_Op.Offset_Image (False, Offset));
                     Offset := Offset + 1;
                  end loop;
               end;
            end if;
         end if;
      end Src_To_Dst;

   begin
      if Op = Op_Identity then
         Src_To_Dst (2);
      elsif Op in Unary_Operator then
         Src_To_Dst (2);
         This.Put_Instruction (Op_Name, Dst_Image);
      elsif Op = Op_Dereference then
         if Src_2_Op.Is_Zero then
            This.Put_Instruction ("CLX");
         else
            This.Put_Instruction ("LDX", Src_2_Image);
         end if;
         This.Put_Instruction ("LDA", Src_1_Image & ",X");
         This.STA (Dst);
      elsif Op = Op_Store then
         This.LDA (Src_2);
         This.STA (Dst);
      elsif Op in Floating_Point_Operator then
         This.Put_Instruction ("LDF", Src_1_Image, "AC0");
         This.Put_Instruction (Op_Name, Src_2_Image, "AC0");
         This.Put_Instruction ("STF", "AC0", Dst_Image);
      else
         if Op in Compare_Operator then
            This.Put_Instruction ("lda", "#0");
            This.Put_Instruction ("sta", "r0");
            This.A := Tagatha.Names.To_Name ("#$00");
            This.LDA (Src_1);
            case Src_1_Op.Content is
               when General_Content =>
                  This.Put_Instruction ("cmp", Src_2_Image);
               when Floating_Point_Content =>
                  This.Put_Instruction ("CMPF", Src_2_Image, Dst_Image);
            end case;
            This.Put_Instruction (Op_Name, "!br+");
            This.Put_Instruction ("inc", "r0");
            This.Name_Label ("!br");
            This.Put_Instruction ("lda", "r0");
            This.STA (Dst_Op);
         elsif Op_Name = "ADD" and then Src_2_Image = "#$01" then
            This.Put_Instruction ("INC", Dst_Image);
            if Tagatha.Names.To_String (This.A) = Dst_Image then
               This.A := Tagatha.Names.Empty_Name;
            end if;
         elsif Op = Op_Divide and then Src_2_Image = "#$02" then
            This.Put_Instruction ("LSR", Dst_Image);
            if Tagatha.Names.To_String (This.A) = Dst_Image then
               This.A := Tagatha.Names.Empty_Name;
            end if;
         elsif Op = Op_Mod and then Src_2_Image = "#$02" then
            This.Put_Instruction ("AND #$01");
         elsif Op = Op_Multiply and then Src_2_Image = "#$03" then
            This.Put_Instruction ("ASL", Dst_Image);
            This.Put_Instruction ("LDA", Dst_Image);
            This.Put_Instruction ("CLC");
            This.Put_Instruction ("ADC", Src_1_Image);
            This.Put_Instruction ("STA", Dst_Image);
         elsif Op = Op_Multiply then
            This.LDA (Dst);
            This.LDX (Src_2);
            This.Put_Instruction ("jsr", "_multiply_8");
            This.STA (Dst);
         else
            This.Put_Instruction (Op_Name, Src_2_Image, Dst_Image);
         end if;
      end if;
   end Transfer;

end Tagatha.Arch.M6502;
