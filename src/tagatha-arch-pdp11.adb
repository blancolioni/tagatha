with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Conversions;
with Tagatha.Names;

package body Tagatha.Arch.Pdp11 is

   function Octal_Image (X : Word_64) return String;

   function Local_Label (L : Positive) return String;

   type No_Operand_Instance is new Pdp11_Operand_Instance with null record;

   overriding function Image (This : No_Operand_Instance) return String
   is ("");

   type Argument_Operand_Instance is new Pdp11_Operand_Instance with
      record
         Index : Argument_Index;
      end record;

   overriding function Image (This : Argument_Operand_Instance) return String;

   type Local_Operand_Instance is new Pdp11_Operand_Instance with
      record
         Index : Local_Index;
      end record;

   overriding function Image (This : Local_Operand_Instance) return String;

   overriding function Offset_Image
     (This        : Local_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String;

   type Result_Operand_Instance is new Pdp11_Operand_Instance with
      record
         Index : Result_Index;
      end record;

   overriding function Image (This : Result_Operand_Instance) return String;

   type Return_Operand_Instance is new Pdp11_Operand_Instance with
      record
         Index : Return_Index;
      end record;

   overriding function Image (This : Return_Operand_Instance) return String;

   type Temporary_Operand_Instance is new Pdp11_Operand_Instance with
      record
         Index       : Temporary_Index;
         R           : Temporary_Register_Index;
         First_Write : Boolean;
         Last_Read   : Boolean;
      end record;

   overriding function Image
     (This : Temporary_Operand_Instance)
      return String;

   type Constant_Operand_Instance is new Pdp11_Operand_Instance with
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
   is ('#' & Octal_Image (This.Value / 2 ** (Word_Offset * 16) mod 65536));

   type External_Operand_Instance is new Pdp11_Operand_Instance with
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
                                  Index   => Index));

   overriding function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Local_Index)
      return Operand_Interface'Class
   is (Local_Operand_Instance'(Content => Content,
                               Index   => Index));

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

   function To_Macro11_Label (Name : String) return String;

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
   begin
      This.Put_Line (To_Macro11_Label (Name) & ":");
      This.Put_Instruction ("MOV", "R5", "-(SP)");
      This.Put_Instruction ("MOV", "SP", "R5");
      if Locals <= 1 then
         for I in 1 .. Locals * 2 loop
            This.Put_Instruction ("TST", "-(SP)");
         end loop;
      else
         declare
            Bytes : constant Natural := Natural (Locals) * 2;
            Img : String := Bytes'Image;
         begin
            Img (Img'First) := '#';
            This.Put_Instruction ("SUB", Img, "SP");
         end;
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
         This.Put_Instruction ("BR", Local_Label (Destination));
      else
         This.Put_Instruction ("TST", Operand.Image);
         This.Put_Instruction
           ((if Condition = Z then "BEQ" else "BNE"),
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
   begin
      for Arg of Actuals loop
         This.Put_Instruction
           ("MOV", Arg.Image, "-(SP)");
      end loop;

      if Name in External_Operand_Instance'Class then
         This.Put_Instruction
           ("JSR", "PC", Name.Image);
      else
         This.Put_Instruction
           ("MOV", Name.Image, "R1");
         This.Put_Instruction
           ("JSR", "PC", "(R1)");
      end if;

      declare
         Count : constant Natural :=
                   Natural (Actuals.Length) * 2;
         Img   : String := Count'Image;
      begin
         Img (Img'First) := '#';
         This.Put_Instruction
           ("ADD", Img, "SP");
      end;

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
      This.Put_Instruction ("MOV", "R5", "SP");
      This.Put_Instruction ("MOV", "(SP)+", "R5");
      This.Put_Instruction ("RTS", "PC");
   end End_Routine;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Argument_Operand_Instance)
      return String
   is
      Offset : constant Integer :=
                 Integer (This.Index) * 2 + 2;
      Img    : constant String := Offset'Image;
   begin
      return Img (2 .. Img'Last) & "(R5)";
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Constant_Operand_Instance)
      return String
   is
   begin
      return "#" & Octal_Image (This.Value);
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : External_Operand_Instance)
      return String
   is
   begin
      return (if This.Address then "#" else "")
        & To_Macro11_Label (Tagatha.Names.To_String (This.Name));
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Local_Operand_Instance)
      return String
   is
      Offset : constant Integer :=
                 Integer (This.Index) * 2;
      Img    : constant String := Offset'Image;
   begin
      return "-" & Img (2 .. Img'Last) & "(R5)";
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Result_Operand_Instance)
      return String
   is
   begin
      if This.Index = 1 then
         return "R0";
      else
         declare
            Offset : constant Integer :=
                       Integer (This.Index - 1) * 2 + 2;
            Img    : constant String := Offset'Image;
         begin
            return Img (2 .. Img'Last) & "(R5)";
         end;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Return_Operand_Instance)
      return String
   is
   begin
      if This.Index = 1 then
         return "R0";
      else
         declare
            Offset : constant Integer :=
                       Integer (This.Index - 1) * 2 + 2;
            Img    : constant String := Offset'Image;
         begin
            return Img (2 .. Img'Last) & "(SP)";
         end;
      end if;
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
                     when General_Content => "R",
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

   -----------------
   -- Local_Label --
   -----------------

   function Local_Label (L : Positive) return String is
      Img : constant String := L'Image;
   begin
      return "L" & Img (2 .. Img'Last);
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
   end Name_Label;

   -----------------
   -- Octal_Image --
   -----------------

   function Octal_Image (X : Word_64) return String is
      It  : Word_64 := X;
      Img : String (1 .. 12) := "000000000000";
      Last : Natural := Img'Last;
   begin
      if It = 0 then
         return "0";
      end if;

      for Ch of reverse Img loop
         Ch := Character'Val (It mod 8 + 48);
         It := It / 8;
         if It = 0 then
            return Img (Last .. Img'Last);
         end if;
         Last := Last - 1;
      end loop;
      return Img;
   end Octal_Image;

   ------------------
   -- Offset_Image --
   ------------------

   overriding function Offset_Image
     (This        : Local_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String
   is
      Offset : constant Integer :=
                 Integer (This.Index) * 2
                 - Word_Offset * 2;
      Img    : constant String := Offset'Image;
   begin
      return "-" & Img (2 .. Img'Last) & "(R5)";
   end Offset_Image;

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

   ----------------------
   -- To_Macro11_Label --
   ----------------------

   function To_Macro11_Label (Name : String) return String is
   begin
      for I in Name'Range loop
         if Name (I) = '.' then
            return Name (Name'First .. I - 1)
              & "__"
              & To_Macro11_Label (Name (I + 1 .. Name'Last));
         end if;
      end loop;
      return Name;
   end To_Macro11_Label;

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
                      when Op_EQ          => "BNE",
                      when Op_NE          => "BEQ",
                      when Op_LT          => "BLE",
                      when Op_LE          => "BLT",
                      when Op_GT          => "BGE",
                      when Op_GE          => "BGT");

      Src_1_Op : Pdp11_Operand_Instance'Class renames
                   Pdp11_Operand_Instance'Class (Src_1);
      Src_2_Op : Pdp11_Operand_Instance'Class renames
                   Pdp11_Operand_Instance'Class (Src_2);
      Dst_Op   : Pdp11_Operand_Instance'Class renames
                   Pdp11_Operand_Instance'Class (Dst);

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
         Src_Op    : constant Pdp11_Operand_Instance'Class :=
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
               This.Put_Instruction
                 ("MOV", Src_Op.Offset_Image (Src => True),
                  Dst_Op.Offset_Image (Src => False));
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
         This.Put_Instruction
           ("MOV",
            Src_2_Image (2 .. Src_2_Image'Last)
            & "(" & Src_1_Image & ")",
            Dst_Image);
      elsif Op = Op_Store then
         This.Put_Instruction
           ("MOV",
            Src_2_Image,
            "(" & Dst_Image & ")");
      elsif Op in Floating_Point_Operator then
         This.Put_Instruction ("LDF", Src_1_Image, "AC0");
         This.Put_Instruction (Op_Name, Src_2_Image, "AC0");
         This.Put_Instruction ("STF", "AC0", Dst_Image);
      else
         Src_To_Dst (1);
         if Op in Compare_Operator then
            This.Put_Instruction ("CLR", "R1");
            case Src_1_Op.Content is
               when General_Content =>
                  This.Put_Instruction ("CMP", Src_2_Image, Dst_Image);
               when Floating_Point_Content =>
                  This.Put_Instruction ("CMPF", Src_2_Image, Dst_Image);
            end case;
            This.Put_Instruction (Op_Name, "1$");
            This.Put_Instruction ("INC", "R1");
            This.Name_Label ("1$");
            This.Put_Instruction ("MOV", "R1", Dst_Image);
         elsif Op_Name = "ADD" and then Src_2_Image = "#1" then
            This.Put_Instruction ("INC", Dst_Image);
         elsif Op_Name = "DIV" and then Src_2_Image = "#2" then
            This.Put_Instruction ("ASR", "#1", Dst_Image);
         else
            This.Put_Instruction (Op_Name, Src_2_Image, Dst_Image);
         end if;
      end if;
   end Transfer;

end Tagatha.Arch.Pdp11;
