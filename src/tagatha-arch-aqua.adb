with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Tagatha.Names;

package body Tagatha.Arch.Aqua is

   function Local_Label
     (L       : Positive;
      Forward : Boolean)
      return String;

   function Register_Image (R : Register_Index) return String
   is ("%" & Ada.Strings.Fixed.Trim (R'Image, Ada.Strings.Left));

   type No_Operand_Instance is new Aqua_Operand_Instance with null record;

   overriding function Image (This : No_Operand_Instance) return String
   is ("");

   type Argument_Operand_Instance is new Aqua_Operand_Instance with
      record
         Index : Argument_Index;
      end record;

   overriding function Image (This : Argument_Operand_Instance) return String
   is (Show (This.R));

   type Local_Operand_Instance is new Aqua_Operand_Instance with
      record
         Index : Local_Index;
      end record;

   overriding function Image (This : Local_Operand_Instance) return String
   is (Show (This.R));

   type Result_Operand_Instance is new Aqua_Operand_Instance with
      record
         Index : Result_Index;
      end record;

   overriding function Image (This : Result_Operand_Instance) return String
   is (Show (This.R));

   type Return_Operand_Instance is new Aqua_Operand_Instance with
      record
         Index : Return_Index;
      end record;

   overriding function Image (This : Return_Operand_Instance) return String
   is (Show (This.R));

   type Temporary_Operand_Instance is new Aqua_Operand_Instance with
      record
         Index       : Temporary_Index;
         First_Write : Boolean;
         Last_Read   : Boolean;
      end record;

   overriding function Image
     (This : Temporary_Operand_Instance)
      return String
   is (Show (This.R));

   type Constant_Operand_Instance is new Aqua_Operand_Instance with
      record
         Value    : Word_64;
      end record;

   overriding function Image (This : Constant_Operand_Instance) return String;

   overriding procedure Move_To_Register
     (Operand     : Constant_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index);

   overriding function Is_Register_Operand
     (This : Constant_Operand_Instance)
      return Boolean
   is (False);

   type External_Operand_Instance is new Aqua_Operand_Instance with
      record
         Name     : Tagatha.Names.Tagatha_Name;
         Imported : Boolean;
         Address  : Boolean;
      end record;

   overriding function Image (This : External_Operand_Instance) return String
   is (Tagatha.Names.To_String (This.Name));

   overriding procedure Move_To_Register
     (Operand     : External_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index);

   overriding function Is_Register_Operand
     (This : External_Operand_Instance)
      return Boolean
   is (False);

   overriding procedure Set_From_Register
     (Operand : External_Operand_Instance;
      This    : in out Instance'Class;
      Source  : Register_Index);

   overriding function No_Operand
     (This  : Instance)
      return Operand_Interface'Class
   is (No_Operand_Instance'(Content => General_Content, R => 0));

   overriding function Argument_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Argument_Index)
      return Operand_Interface'Class
   is (Argument_Operand_Instance'
         (Content => Content,
          R       => This.First_Arg + Register_Index (Index) - 1,
          Index   => Index));

   overriding function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Local_Index)
      return Operand_Interface'Class
   is (Local_Operand_Instance'
         (Content => Content,
          R       => This.First_Local + Register_Index (Index) - 1,
          Index   => Index));

   overriding function Result_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Result_Index)
      return Operand_Interface'Class
   is (Result_Operand_Instance'
         (Content => Content,
          R       => This.First_Result + Register_Index (Index) - 1,
          Index   => Index));

   overriding function Return_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Return_Index)
      return Operand_Interface'Class
   is (Return_Operand_Instance'
         (Content => Content,
          R       => This.Call_Return + Register_Index (Index) - 1,
          Index   => Index));

   overriding function Constant_Operand
     (This     : Instance;
      Content  : Operand_Content;
      Value    : Word_64)
      return Operand_Interface'Class
   is (Constant_Operand_Instance'
         (Content  => Content,
          R        => 0,
          Value    => Value));

   overriding function Name_Operand
     (This    : Instance;
      Name    : String;
      Address  : Boolean;
      Imported : Boolean)
      return Operand_Interface'Class
   is (External_Operand_Instance'
         (Content  => General_Content,
          R        => 0,
          Name     => Tagatha.Names.To_Name (Name),
          Imported => Imported,
          Address  => Address));

   ----------------
   -- Begin_Data --
   ----------------

   overriding procedure Begin_Data
     (This       : in out Instance;
      Name       : String;
      Bits       : Natural;
      Read_Write : Boolean)
   is
   begin
      if Read_Write then
         This.Put_Instruction ("data");
      end if;
      Parent (This).Begin_Data (Name, Bits, Read_Write);
   end Begin_Data;

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
      This.Put_Line (Name & ":");
      This.First_Arg := 0;
      This.Arg_Bound := This.First_Arg + Register_Index (Arguments);
      This.First_Result := This.Arg_Bound;
      This.Result_Bound := This.First_Result + Register_Index (Results);
      This.First_Local := This.Result_Bound;
      This.Local_Bound := This.First_Local + Register_Index (Locals);
      This.First_Temp := This.Local_Bound;
      This.Temp_Bound := This.First_Temp;
      This.Linkage := Linkage;
      if Linkage then
         This.Saved_J := This.Claim;
         This.Put_Instruction ("get", Register_Image (This.Saved_J), "rJ");
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
      Op          : Aqua_Operand_Instance'Class renames
                      Aqua_Operand_Instance'Class (Operand);
      Is_Register : constant Boolean := Op.Is_Register_Operand;
      R           : constant Register_Index :=
                      (if Condition = Always
                       then 0
                       elsif Is_Register
                       then Op.R
                       else This.Claim);
   begin
      if Condition = Always then
         This.Put_Instruction ("jmp", Local_Label (Destination, Forward));
      else
         if not Is_Register then
            Op.Move_To_Register (This, R);
         end if;

         This.Put_Instruction
           ((if Condition = Z then "bz" else "bnz"),
            Register_Image (R),
            Local_Label (Destination, Forward));

         if not Is_Register then
            This.Release (R);
         end if;
      end if;
   end Branch;

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (This           : in out Instance;
      Name           : Operand_Interface'Class;
      Actuals        : Operand_Lists.List;
      Result_Count   : Natural)
   is
      Push_Arg : constant Register_Index :=
                   Register_Index'Max (This.Temp_Bound, 1);
      Arg_Reg  : Register_Index := This.Temp_Bound;
   begin
      for Arg of Actuals loop
         Arg_Reg := Arg_Reg + 1;
         Aqua_Operand_Instance'Class (Arg).Move_To_Register (This, Arg_Reg);
      end loop;

      This.Call_Return :=
        Register_Index'Max (This.Temp_Bound, 1);

      if Name in External_Operand_Instance'Class then
         This.Put_Instruction
           ("pushj", Register_Image (Push_Arg), Name.Image);
      else
         This.Put_Instruction
           ("pushgo", Register_Image (Push_Arg),
            Register_Image (Aqua_Operand_Instance'Class (Name).R), "0");
      end if;

   end Call;

   -----------
   -- Claim --
   -----------

   function Claim (This : in out Instance'Class) return Register_Index is
   begin
      for R in This.First_Temp .. Last_Register loop
         if This.Temps (R).Assignment = 0
           and then not This.Temps (R).Claimed
         then
            This.Temps (R).Claimed := True;
            This.Temp_Bound := Register_Index'Max (This.Temp_Bound, R + 1);
            return R;
         end if;
      end loop;
      raise Constraint_Error with
        "Claim: no available temporaries";
   end Claim;

   --------------
   -- End_Data --
   --------------

   overriding procedure End_Data
     (This : in out Instance)
   is
   begin
      Parent (This).End_Data;
      if This.RW_Data then
         This.Put_Instruction ("code");
      end if;
   end End_Data;

   -----------------
   -- End_Routine --
   -----------------

   overriding procedure End_Routine
     (This : in out Instance)
   is
   begin
      if This.Linkage then
         This.Put_Instruction ("put", "rJ", Register_Image (This.Saved_J));
         if This.First_Result > 0 then
            for R in This.First_Result .. This.Result_Bound - 1 loop
               This.Put_Instruction
                 ("set", Register_Image (R - This.First_Result),
                  Register_Image (R));
            end loop;
         end if;
         This.Release (This.Saved_J);
         This.Put_Instruction
           ("pop",
            Register_Index'Image
              (Register_Index'Max (This.Result_Bound - This.First_Result, 1)),
            "0");
         This.Temps := [others => <>];
      end if;

      while This.Last_Ind_Written < This.Indirect_Vector.Last_Index loop
         This.Last_Ind_Written := @ + 1;
         declare
            Index_Image : String := This.Last_Ind_Written'Image;
         begin
            Index_Image (Index_Image'First) := '_';
            declare
               Indirect_Label : constant String :=
                                  "_ext_indirect" & Index_Image;
            begin
               This.Begin_Data (Indirect_Label, 32, False);
               This.Label_Datum (This.Indirect_Vector (This.Last_Ind_Written));
               This.End_Data;
            end;
         end;
      end loop;

   end End_Routine;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Constant_Operand_Instance)
      return String
   is
      Img : constant String := This.Value'Image;
   begin
      return Img (2 .. Img'Last);
   end Image;

   --------------------
   -- Indirect_Label --
   --------------------

   function Indirect_Label
     (This           : in out Instance;
      External_Label : String)
      return String
   is
      Index : Natural :=
                This.Indirect_Vector.Find_Index (External_Label);
   begin
      if Index = Indirect_Label_Vectors.No_Index then
         This.Indirect_Vector.Append (External_Label);
         Index := This.Indirect_Vector.Last_Index;
      end if;

      pragma Assert (Index > 0);

      declare
         Index_Image : String := Index'Image;
      begin
         Index_Image (Index_Image'First) := '_';
         return "_ext_indirect" & Index_Image;
      end;

   end Indirect_Label;

   ----------
   -- Jump --
   ----------

   overriding procedure Jump
     (This           : in out Instance;
      Name           : Operand_Interface'Class)
   is
   begin
      This.Put_Instruction ("jmp", Name.Image);
   end Jump;

   -----------------
   -- Local_Label --
   -----------------

   function Local_Label
     (L       : Positive;
      Forward : Boolean)
      return String
   is
      Img : constant String := L'Image;
   begin
      return Img (2 .. Img'Last) & (if Forward then "f" else "b");
   end Local_Label;

   -----------------
   -- Local_Label --
   -----------------

   overriding procedure Local_Label
     (This  : in out Instance;
      Label : Positive)
   is
      Img : constant String := Label'Image;
   begin
      This.Put_Line (Img (2 .. Img'Last));
   end Local_Label;

   ----------------------
   -- Move_To_Register --
   ----------------------

   procedure Move_To_Register
     (Operand     : Aqua_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index)
   is
      Src_Image : constant String :=
                    Aqua_Operand_Instance'Class (Operand).Image;
      pragma Assert (Src_Image /= "",
                     "no image for " & Operand'Image);
   begin
      This.Put_Instruction
        ("set", Register_Image (Destination), Src_Image);
   end Move_To_Register;

   ----------------------
   -- Move_To_Register --
   ----------------------

   overriding procedure Move_To_Register
     (Operand     : Constant_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index)
   is
      Lo : constant Word_64 := Operand.Value mod 65536;
      Hi : constant Word_64 := Operand.Value / 65536;
   begin
      if Operand.Value = 0 or else Lo /= 0 then
         This.Put_Instruction
           ("setl", Register_Image (Destination), Lo'Image);
         if Hi /= 0 then
            This.Put_Instruction
              ("inch", Register_Image (Destination), Hi'Image);
         end if;
      else
         This.Put_Instruction
           ("seth", Register_Image (Destination), Hi'Image);
      end if;
   end Move_To_Register;

   ----------------------
   -- Move_To_Register --
   ----------------------

   overriding procedure Move_To_Register
     (Operand     : External_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index)
   is
      Label          : constant String :=
                         Tagatha.Names.To_String (Operand.Name);
   begin
      if Operand.Imported then
         declare
            R              : constant Register_Index := This.Claim;
            Indirect_Label : constant String := This.Indirect_Label (Label);
         begin
            This.Put_Instruction
              ("geta", Register_Image (R), Indirect_Label);
            if not Operand.Address then
               This.Put_Instruction
                 ("ld", Register_Image (R), Register_Image (R), "0");
            end if;

            This.Put_Instruction ("ld", Register_Image (Destination),
                                  Register_Image (R), "0");
            This.Release (R);
         end;
      elsif Operand.Address then
         This.Put_Instruction
           ("geta", Register_Image (Destination), Label);
      else
         declare
            R              : constant Register_Index := This.Claim;
         begin
            This.Put_Instruction
              ("geta", Register_Image (R), Label);
            This.Put_Instruction ("ld", Register_Image (Destination),
                                  Register_Image (R), "0");
            This.Release (R);
         end;
      end if;
   end Move_To_Register;

   ----------------
   -- Name_Label --
   ----------------

   overriding procedure Name_Label
     (This : in out Instance;
      Name : String)
   is
   begin
      This.Put_Line (Name);
   end Name_Label;

   ---------------------
   -- Put_Data_Buffer --
   ---------------------

   overriding procedure Put_Data_Buffer (This : in out Instance) is
      use Ada.Strings.Unbounded;
      S     : Unbounded_String;
      First : Boolean := True;
   begin

      if This.Data_Bits <= 8 then
         S := To_Unbounded_String ("    byte ");
      else
         S := To_Unbounded_String ("    word ");
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

   -------------
   -- Release --
   -------------

   procedure Release
     (This : in out Instance'Class;
      R    : Register_Index)
   is
   begin
      This.Temps (R).Claimed := False;
   end Release;

   -----------------------
   -- Set_From_Register --
   -----------------------

   procedure Set_From_Register
     (Operand : Aqua_Operand_Instance;
      This    : in out Instance'Class;
      Source  : Register_Index)
   is
   begin
      This.Put_Instruction
        ("set", Aqua_Operand_Instance'Class (Operand).Image,
         Register_Image (Source));
   end Set_From_Register;

   -----------------------
   -- Set_From_Register --
   -----------------------

   overriding procedure Set_From_Register
     (Operand : External_Operand_Instance;
      This    : in out Instance'Class;
      Source  : Register_Index)
   is
      R              : constant Register_Index := This.Claim;
      Label          : constant String :=
                         Tagatha.Names.To_String (Operand.Name);
   begin
      if Operand.Imported then
         declare
            Indirect_Label : constant String := This.Indirect_Label (Label);
         begin
            This.Put_Instruction
              ("geta", Register_Image (R), Indirect_Label);
            This.Put_Instruction
              ("ld", Register_Image (R), Register_Image (R), "0");
         end;
      else
         This.Put_Instruction
           ("geta", Register_Image (R), Label);
      end if;
      This.Put_Instruction
        ("st", Register_Image (Source), Register_Image (R), "0");
      This.Release (R);
   end Set_From_Register;

   ----------
   -- Show --
   ----------

   function Show (Index : Register_Index) return String is
      Img : constant String := Index'Image;
   begin
      return "%" & Img (2 .. Img'Last);
   end Show;

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
      R : Register_Index := This.First_Temp;
   begin
      loop
         declare
            State : Register_State renames This.Temps (R);
         begin
            if not State.Claimed then
               if First_Write then
                  if State.Assignment = 0 then
                     State.Assignment := Index;
                     This.Temp_Bound :=
                       Register_Index'Max (This.Temp_Bound, R + 1);
                     exit;
                  end if;
               else
                  if State.Assignment = Index then
                     if Last_Read then
                        State.Assignment := 0;
                     end if;
                     exit;
                  end if;
               end if;
            end if;
         end;
         if R = Last_Register then
            declare
               Message : constant String :=
                           (if First_Write
                            then "no spare registers for temporary"
                            & Index'Image
                            else "attempt to read temporary" & Index'Image
                            & " which has not been written");
            begin

               Ada.Text_IO.Put_Line (Message);

               for T in This.First_Temp .. This.Temp_Bound - 1 loop
                  Ada.Text_IO.Put_Line
                    (Register_Image (T) & " -> "
                     & (if This.Temps (T).Claimed
                       then "claimed"
                       else "t" & Integer'Image
                         (-Integer (This.Temps (T).Assignment))));
               end loop;

               raise Constraint_Error with Message;
            end;
         end if;
         R := R + 1;
      end loop;

      return Temporary_Operand_Instance'
        (R, Content, Index, First_Write, Last_Read);

   end Temporary_Operand;

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
                      when Op_Negate      => "neg",
                      when Op_Not         => "not",
                      when Op_Test        => "",
                      when Op_Add         => "add",
                      when Op_Subtract    => "sub",
                      when Op_Multiply    => "mul",
                      when Op_Divide      => "div",
                      when Op_Mod         => "mod",
                      when Op_Fadd        => "fadd",
                      when Op_Fsub        => "fsub",
                      when Op_Fmul        => "fmul",
                      when Op_Fdiv        => "fdiv",
                      when Op_And         => "and",
                      when Op_Or          => "or",
                      when Op_Xor         => "xor",
                      when Op_Dereference => "",
                      when Op_Store       => "",
                      when Op_EQ          => "zsz",
                      when Op_NE          => "zsnz",
                      when Op_LT          => "zsn",
                      when Op_LE          => "zsnp",
                      when Op_GT          => "zsp",
                      when Op_GE          => "zsnn");

      Dst_Op      : Aqua_Operand_Instance'Class renames
                      Aqua_Operand_Instance'Class (Dst);
      Src_1_Op    : Aqua_Operand_Instance'Class renames
                      Aqua_Operand_Instance'Class (Src_1);
      Src_2_Op    : Aqua_Operand_Instance'Class renames
                      Aqua_Operand_Instance'Class (Src_2);
      Src_1_Image : constant String := Src_1.Image;
      Src_2_Image : constant String := Src_2.Image;
      Dst_Image   : constant String := Dst.Image;
   begin
      if Op = Op_Identity then
         if Src_2_Image /= Dst_Image then
            if Dst_Op.Is_Register_Operand then
               Src_2_Op.Move_To_Register (This, Dst_Op.R);
            else
               declare
                  T : constant Register_Index := This.Claim;
               begin
                  Src_2_Op.Move_To_Register (This, T);
                  Dst_Op.Set_From_Register (This, T);
                  This.Release (T);
               end;
            end if;
         end if;
      elsif Op = Op_Test then
         null;
      elsif Op = Op_Not then
         This.Put_Instruction ("zsz", Dst_Image, Src_2_Image, "1");
      elsif Op = Op_Negate then
         This.Put_Instruction ("neg", Dst_Image, "0", Src_2_Image);
      elsif Op in Unary_Operator then
         This.Put_Instruction (Op_Name, Dst_Image, Src_2_Image);
      elsif Op = Op_Dereference then
         declare
            Offset : constant Word_64 :=
                       Constant_Operand_Instance'Class (Src_2).Value;
         begin
            if not Src_1_Op.Is_Register_Operand then
               declare
                  R : constant Register_Index := This.Claim;
               begin
                  Src_1_Op.Move_To_Register (This, R);
                  This.Put_Instruction
                    ("ld", Dst_Image, Register_Image (R), Offset'Image);
                  This.Release (R);
               end;
            else
               This.Put_Instruction
                 ("ld", Dst_Image, Src_1_Image, Offset'Image);
            end if;
         end;
      elsif Op = Op_Store then
         declare
            Offset : constant Word_64 :=
                       Constant_Operand_Instance'Class (Src_2).Value;
            T      : constant Register_Index := This.Claim;
         begin
            Src_1_Op.Move_To_Register (This, T);
            This.Put_Instruction
              ("st", Register_Image (T),
               Register_Image (Dst_Op.R), Offset'Image);
            This.Release (T);
         end;
      elsif Op = Op_Mod then
         declare
            R : constant Register_Index := This.Claim;
         begin
            This.Put_Instruction ("div", Register_Image (R),
                                  Src_1_Image, Src_2_Image);
            This.Put_Instruction ("get", Register_Image (R), "rR");
            Dst_Op.Set_From_Register (This, R);
            This.Release (R);
         end;
      else
         declare
            procedure Put (Dst, Src_1, Src_2 : String);

            ---------
            -- Put --
            ---------

            procedure Put (Dst, Src_1, Src_2 : String) is
            begin
               if Op in Compare_Operator then
                  This.Put_Instruction ("sub", Dst, Src_1, Src_2);
                  This.Put_Instruction (Op_Name, Dst, Dst, "1");
               else
                  This.Put_Instruction (Op_Name, Dst, Src_1, Src_2);
               end if;
            end Put;

         begin

            if not Src_2_Op.Is_Register_Operand
              and then (Src_2_Op not in Constant_Operand_Instance'Class
                        or else Constant_Operand_Instance'Class (Src_2_Op)
                        .Value >= 256)
            then
               declare
                  R : constant Register_Index := This.Claim;
               begin
                  Src_2_Op.Move_To_Register (This, R);
                  Put (Dst_Image, Src_1_Image, Register_Image (R));
                  --  This.Put_Instruction (Op_Name, Dst_Image, Src_1_Image,
                  --                        Register_Image (R));
                  This.Release (R);
               end;
            else
               Put (Dst_Image, Src_1_Image, Src_2_Image);
               --  This.Put_Instruction
               --    (Op_Name, Dst_Image, Src_1_Image, Src_2_Image);
            end if;
         end;
      end if;
   end Transfer;

end Tagatha.Arch.Aqua;
