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
          R       => This.Arg_Reg (Index),
          Index   => Index));

   overriding function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Local_Index)
      return Operand_Interface'Class
   is (Local_Operand_Instance'
         (Content => Content,
          R       => This.Local_Reg (Index),
          Index   => Index));

   overriding function Result_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Result_Index)
      return Operand_Interface'Class
   is (Result_Operand_Instance'
         (Content => Content,
          R       => This.Result_Reg (Index),
          Index   => Index));

   overriding function Return_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Return_Index)
      return Operand_Interface'Class
   is (Return_Operand_Instance'
         (Content => Content,
          R       => This.Return_Reg (Index),
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
      Linkage   : Boolean;
      Layout    : Frame_Layout)
   is
      --  Arguments, then results, then locals, then temporaries, each slot
      --  taking Slot_Width registers.  A double displaces everything after
      --  it, so the index -> register maps are prefix sums rather than
      --  First_X + Index - 1.
      Next : Register_Index := 0;
   begin
      This.Put_Line (Name & ":");

      This.First_Arg := Next;
      for I in 1 .. Arguments loop
         This.Arg_Reg (I) := Next;
         Next := Next + Slot_Width (Layout.Arguments (I));
      end loop;
      This.Arg_Bound := Next;

      This.First_Result := Next;
      for I in 1 .. Results loop
         This.Result_Reg (I) := Next;
         Next := Next + Slot_Width (Layout.Results (I));
      end loop;
      This.Result_Bound := Next;

      This.First_Local := Next;
      for I in 1 .. Locals loop
         This.Local_Reg (I) := Next;
         Next := Next + Slot_Width (Layout.Locals (I));
      end loop;
      This.Local_Bound := Next;

      This.First_Temp := Next;
      This.Temp_Bound := Next;
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
      Result_Count   : Natural;
      Returns        : Return_Content_Array)
   is
      Push_Arg : constant Register_Index :=
                   Register_Index'Max (This.Temp_Bound, 1);
      Arg_Reg  : Register_Index := This.Temp_Bound + 1;
   begin
      --  Actuals go in the registers above Push_Arg, so that pushj shifts the
      --  window to put the first actual at the callee's %0.  A double actual
      --  fills the pair (Arg_Reg, Arg_Reg + 1) -- Move_To_Register copies both
      --  halves -- so the next actual starts two registers later.
      for Arg of Actuals loop
         declare
            Actual : Aqua_Operand_Instance'Class renames
                       Aqua_Operand_Instance'Class (Arg);
         begin
            Actual.Move_To_Register (This, Arg_Reg);
            Arg_Reg := Arg_Reg + Slot_Width (Actual.Content);
         end;
      end loop;

      This.Call_Return :=
        Register_Index'Max (This.Temp_Bound, 1);

      --  Return values come back at Call_Return upwards, laid out by the same
      --  prefix sum the callee used for its results.
      declare
         Next : Register_Index := This.Call_Return;
      begin
         for I in Return_Count'(1) .. Return_Count (Result_Count) loop
            This.Return_Reg (I) := Next;
            Next := Next + Slot_Width (Returns (I));
         end loop;
      end;

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

   ----------------
   -- Claim_Pair --
   ----------------

   function Claim_Pair (This : in out Instance'Class) return Register_Index is
   begin
      for R in This.First_Temp .. Last_Register - 1 loop
         if not This.Temps (R).Claimed
           and then This.Temps (R).Assignment = 0
           and then not This.Temps (R + 1).Claimed
           and then This.Temps (R + 1).Assignment = 0
         then
            This.Temps (R).Claimed := True;
            This.Temps (R + 1).Claimed := True;
            This.Temp_Bound := Register_Index'Max (This.Temp_Bound, R + 2);
            return R;
         end if;
      end loop;
      raise Constraint_Error with
        "Claim_Pair: no available temporaries";
   end Claim_Pair;

   -----------
   -- Datum --
   -----------

   overriding procedure Datum
     (This  : in out Instance;
      Value : Word_64)
   is
   begin
      if This.Data_Bits = 64 then
         --  double: two 32-bit words, high word first
         Parent (This).Datum (Value / 2 ** 32);
         Parent (This).Datum (Value mod 2 ** 32);
      else
         Parent (This).Datum (Value);
      end if;
   end Datum;

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
         This.Release (This.Saved_J);
      end if;

      This.Temps := [others => <>];

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

   ------------------
   -- Exit_Routine --
   ------------------

   overriding procedure Exit_Routine
     (This : in out Instance)
   is
      --  Width of the whole result region in REGISTERS, not slots: a double
      --  result counts two.
      Width : constant Register_Index :=
                This.Result_Bound - This.First_Result;
   begin
      if This.Linkage then
         This.Put_Instruction ("put", "rJ", Register_Image (This.Saved_J));

         --  `pop n` moves exactly ONE register into the caller's hole -- the
         --  callee's %(n-1) -- and the remaining n-1 become visible only
         --  because the window base shifts down by one.  So after `pushj %H`
         --  the caller sees
         --
         --     %H     <- callee %(n-1)
         --     %(H+k) <- callee %(k-1)   for k in 1 .. n-1
         --
         --  i.e. the callee's %0 .. %(n-1) arrive rotated left by one.  For a
         --  single-register result the rotation is the identity, which is why
         --  it went unnoticed; for a double it hands the caller the low word
         --  first, at %H, with the high word above it -- reversed, and not a
         --  usable (R, R + 1) pair.
         --
         --  So rotate here, on the way out: word 0 goes to %(Width - 1) and
         --  word w to %(w - 1).  The caller then sees the words in order and
         --  contiguous from %H, which is what Call's Return_Reg prefix sum
         --  assumes.
         --
         --  This also relies on the callee's rL exceeding Width: `pop n` reads
         --  the hole value with Get_R (n), which returns 0 for a marginal
         --  register when rL = n exactly.  Linkage guarantees it -- Saved_J is
         --  claimed at or above First_Temp >= Result_Bound -- and Exit_Routine
         --  only emits a pop when Linkage is set.
         if Width = 1 then
            if This.First_Result /= 0 then
               This.Put_Instruction
                 ("set", Register_Image (0),
                  Register_Image (This.First_Result));
            end if;
         elsif Width > 1 then
            declare
               Saved : constant Register_Index := This.Claim;
            begin
               --  Word 0 has to survive the shift below, which may overwrite
               --  it when the result region starts at %0 (a routine with no
               --  arguments).
               This.Put_Instruction
                 ("set", Register_Image (Saved),
                  Register_Image (This.First_Result));

               --  Ascending is safe: destination %D is written at step D, and
               --  a later step reads First_Result + step + 1, which is always
               --  above any destination already written.
               for D in 0 .. Width - 2 loop
                  This.Put_Instruction
                    ("set", Register_Image (D),
                     Register_Image (This.First_Result + D + 1));
               end loop;

               This.Put_Instruction
                 ("set", Register_Image (Width - 1),
                  Register_Image (Saved));
               This.Release (Saved);
            end;
         end if;

         This.Put_Instruction
           ("pop",
            Register_Index'Image (Register_Index'Max (Width, 1)),
            "0");
      end if;

   end Exit_Routine;

   ------------------
   -- Fail_Routine --
   ------------------

   overriding procedure Fail_Routine
     (This        : in out Instance)
   is
   begin
      This.Put_Instruction ("set", "%255", Register_Image (This.Saved_J));
      This.Put_Instruction ("geta", "%253", "1f");
      This.Put_Instruction ("put", "rJ", "%253");
      This.Put_Instruction ("pop", "0", "0");
      This.Local_Label (1);
      This.Put_Instruction ("pushj", "%200", "system.exceptions.fail_handler");
   end Fail_Routine;

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
      if Operand.Content = Floating_Point_Content then
         --  register-backed double: copy both halves of the pair
         if Operand.R /= Destination then
            This.Put_Instruction
              ("set", Register_Image (Destination),
               Register_Image (Operand.R));
            This.Put_Instruction
              ("set", Register_Image (Destination + 1),
               Register_Image (Operand.R + 1));
         end if;
      elsif Src_Image /= Register_Image (Destination) then
         This.Put_Instruction
           ("set", Register_Image (Destination), Src_Image);
      end if;
   end Move_To_Register;

   ----------------------
   -- Move_To_Register --
   ----------------------

   overriding procedure Move_To_Register
     (Operand     : Constant_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index)
   is
      procedure Load (R : Register_Index; Value : Word_64);

      ----------
      -- Load --
      ----------

      procedure Load (R : Register_Index; Value : Word_64) is
         Lo : constant Word_64 := Value mod 65536;
         Hi : constant Word_64 := Value / 65536;
      begin
         if Value = 0 or else Lo /= 0 then
            This.Put_Instruction
              ("setl", Register_Image (R), Lo'Image);
            if Hi /= 0 then
               This.Put_Instruction
                 ("inch", Register_Image (R), Hi'Image);
            end if;
         else
            This.Put_Instruction
              ("seth", Register_Image (R), Hi'Image);
         end if;
      end Load;

   begin
      if Operand.Content = Floating_Point_Content then
         --  binary64 bit pattern: high word in R, low word in R + 1
         Load (Destination, Operand.Value / 2 ** 32);
         Load (Destination + 1, Operand.Value mod 2 ** 32);
      else
         Load (Destination, Operand.Value);
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

   ---------------------
   -- Raise_Exception --
   ---------------------

   overriding procedure Raise_Exception
     (This    : in out Instance;
      E       : Operand_Interface'Class)
   is
   begin
      Aqua_Operand_Instance'Class (E).Move_To_Register (This, 254);
      This.Put_Instruction ("set", "%255", Register_Image (This.Saved_J));
      This.Put_Instruction ("pushj", "%200",
                            "system.exceptions.raise_handler");
   end Raise_Exception;

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

   ------------------
   -- Release_Pair --
   ------------------

   procedure Release_Pair
     (This : in out Instance'Class;
      R    : Register_Index)
   is
   begin
      This.Temps (R).Claimed := False;
      This.Temps (R + 1).Claimed := False;
   end Release_Pair;

   -----------
   -- Retry --
   -----------

   overriding procedure Retry
     (This        : in out Instance;
      Destination : String)
   is
   begin
      This.Put_Instruction ("put", "rJ", Register_Image (This.Saved_J));
      This.Put_Instruction ("jmp", Destination);
   end Retry;

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
      Is_Pair : constant Boolean := Content = Floating_Point_Content;
      R       : Register_Index := This.First_Temp;
   begin
      loop
         declare
            State : Register_State renames This.Temps (R);
         begin
            if Is_Pair then
               --  doubles need the pair (R, R + 1); R holds the high word
               if R < Last_Register
                 and then not State.Claimed
                 and then not This.Temps (R + 1).Claimed
               then
                  declare
                     Next : Register_State renames This.Temps (R + 1);
                  begin
                     if First_Write then
                        if State.Assignment = 0
                          and then Next.Assignment = 0
                        then
                           State.Assignment := Index;
                           Next.Assignment := Index;
                           This.Temp_Bound :=
                             Register_Index'Max (This.Temp_Bound, R + 2);
                           exit;
                        end if;
                     else
                        if State.Assignment = Index then
                           if Last_Read then
                              State.Assignment := 0;
                              Next.Assignment := 0;
                           end if;
                           exit;
                        end if;
                     end if;
                  end;
               end if;
            elsif not State.Claimed then
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

      Dst_Float   : constant Boolean :=
                      Dst_Op.Content = Floating_Point_Content;
      Src_Float   : constant Boolean :=
                      Src_1_Op.Content = Floating_Point_Content
                        or else Src_2_Op.Content = Floating_Point_Content;

      procedure Materialise_Pair
        (Operand : Aqua_Operand_Instance'Class;
         R       : out Register_Index;
         Claimed : out Boolean);
      --  ensure a float operand is in a register pair (R, R + 1);
      --  R holds the high word, Claimed is True if we claimed it here

      --  A temporary read for the last time is freed before Transfer is
      --  called, but its value is still needed until the instruction has
      --  been emitted.  Guard the registers of the incoming operands so
      --  that Claim/Claim_Pair during emission cannot take them.
      Guarded : array (Register_Index) of Boolean := [others => False];

      procedure Guard (Operand : Aqua_Operand_Instance'Class);
      procedure Release_Guards;

      -----------
      -- Guard --
      -----------

      procedure Guard (Operand : Aqua_Operand_Instance'Class) is
         procedure Mark (R : Register_Index);

         ----------
         -- Mark --
         ----------

         procedure Mark (R : Register_Index) is
         begin
            if R in This.First_Temp .. Last_Register
              and then not This.Temps (R).Claimed
              and then This.Temps (R).Assignment = 0
            then
               This.Temps (R).Claimed := True;
               Guarded (R) := True;
            end if;
         end Mark;

      begin
         if Operand.Is_Register_Operand then
            Mark (Operand.R);
            if Operand.Content = Floating_Point_Content
              and then Operand.R < Last_Register
            then
               Mark (Operand.R + 1);
            end if;
         end if;
      end Guard;

      --------------------
      -- Release_Guards --
      --------------------

      procedure Release_Guards is
      begin
         for R in Guarded'Range loop
            if Guarded (R) then
               This.Temps (R).Claimed := False;
            end if;
         end loop;
      end Release_Guards;

      ----------------------
      -- Materialise_Pair --
      ----------------------

      procedure Materialise_Pair
        (Operand : Aqua_Operand_Instance'Class;
         R       : out Register_Index;
         Claimed : out Boolean)
      is
      begin
         if Operand.Is_Register_Operand then
            R := Operand.R;
            Claimed := False;
         else
            R := This.Claim_Pair;
            Operand.Move_To_Register (This, R);
            Claimed := True;
         end if;
      end Materialise_Pair;

   begin
      Guard (Src_1_Op);
      Guard (Src_2_Op);
      Guard (Dst_Op);

      if Op = Op_Identity then
         if Dst_Float and then not Src_Float then
            --  integer to float conversion (D3: Content mismatch)
            pragma Assert (Dst_Op.Is_Register_Operand,
                           "flot: destination must be a register pair");
            if Src_2_Op.Is_Register_Operand
              or else (Src_2_Op in Constant_Operand_Instance'Class
                       and then Constant_Operand_Instance'Class (Src_2_Op)
                         .Value < 256)
            then
               This.Put_Instruction ("flot", Dst_Image, Src_2_Image);
            else
               declare
                  T : constant Register_Index := This.Claim;
               begin
                  Src_2_Op.Move_To_Register (This, T);
                  This.Put_Instruction
                    ("flot", Dst_Image, Register_Image (T));
                  This.Release (T);
               end;
            end if;
         elsif Src_Float and then not Dst_Float then
            --  D3: float to integer conversion.  Signed by default;
            --  fixu needs signedness threaded through the IR.
            declare
               V       : Register_Index;
               Claimed : Boolean;
            begin
               Materialise_Pair (Src_2_Op, V, Claimed);
               if Dst_Op.Is_Register_Operand then
                  This.Put_Instruction
                    ("fix", Dst_Image, Register_Image (V));
               else
                  declare
                     T : constant Register_Index := This.Claim;
                  begin
                     This.Put_Instruction
                       ("fix", Register_Image (T), Register_Image (V));
                     Dst_Op.Set_From_Register (This, T);
                     This.Release (T);
                  end;
               end if;
               if Claimed then
                  This.Release_Pair (V);
               end if;
            end;
         elsif Src_2_Image /= Dst_Image then
            if Dst_Op.Is_Register_Operand then
               Src_2_Op.Move_To_Register (This, Dst_Op.R);
            elsif Src_Float then
               raise Constraint_Error with
                 "aqua: float store to named object not implemented";
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
         if Dst_Float or else Src_Float then
            --  D4: negate a double by flipping the sign bit of the
            --  high word; there is no fneg opcode
            pragma Assert (Dst_Op.Is_Register_Operand,
                           "float negate: destination must be a register");
            if not (Src_2_Op.Is_Register_Operand
                    and then Src_2_Op.R = Dst_Op.R)
            then
               Src_2_Op.Move_To_Register (This, Dst_Op.R);
            end if;
            declare
               Mask : constant Register_Index := This.Claim;
            begin
               This.Put_Instruction
                 ("seth", Register_Image (Mask), "32768");
               This.Put_Instruction
                 ("xor", Dst_Image, Dst_Image, Register_Image (Mask));
               This.Release (Mask);
            end;
         else
            This.Put_Instruction ("neg", Dst_Image, "0", Src_2_Image);
         end if;
      elsif Op in Unary_Operator then
         This.Put_Instruction (Op_Name, Dst_Image, Src_2_Image);
      elsif Op = Op_Dereference then
         declare
            Offset    : constant Word_64 :=
                          Constant_Operand_Instance'Class (Src_2).Value;
            Lo_Offset : constant Word_64 := Offset + 4;

            procedure Load (Base : String);
            --  one 32-bit load, or two for a double (high word at
            --  the lower address)

            ----------
            -- Load --
            ----------

            procedure Load (Base : String) is
            begin
               This.Put_Instruction ("ld", Dst_Image, Base, Offset'Image);
               if Dst_Float then
                  This.Put_Instruction
                    ("ld", Register_Image (Dst_Op.R + 1), Base,
                     Lo_Offset'Image);
               end if;
            end Load;

         begin
            if not Src_1_Op.Is_Register_Operand then
               declare
                  R : constant Register_Index := This.Claim;
               begin
                  Src_1_Op.Move_To_Register (This, R);
                  Load (Register_Image (R));
                  This.Release (R);
               end;
            else
               Load (Src_1_Image);
            end if;
         end;
      elsif Op = Op_Store then
         declare
            Offset       : constant Word_64 :=
                             Constant_Operand_Instance'Class (Src_2).Value;
            Base         : Register_Index;
            Base_Claimed : Boolean := False;
         begin
            if Dst_Op.Is_Register_Operand then
               Base := Dst_Op.R;
            else
               Base := This.Claim;
               Dst_Op.Move_To_Register (This, Base);
               Base_Claimed := True;
            end if;

            if Src_1_Op.Content = Floating_Point_Content then
               declare
                  Lo_Offset : constant Word_64 := Offset + 4;
                  V         : Register_Index;
                  Claimed   : Boolean;
               begin
                  Materialise_Pair (Src_1_Op, V, Claimed);
                  This.Put_Instruction
                    ("st", Register_Image (V),
                     Register_Image (Base), Offset'Image);
                  This.Put_Instruction
                    ("st", Register_Image (V + 1),
                     Register_Image (Base), Lo_Offset'Image);
                  if Claimed then
                     This.Release_Pair (V);
                  end if;
               end;
            else
               declare
                  T : constant Register_Index := This.Claim;
               begin
                  Src_1_Op.Move_To_Register (This, T);
                  This.Put_Instruction
                    ("st", Register_Image (T),
                     Register_Image (Base), Offset'Image);
                  This.Release (T);
               end;
            end if;

            if Base_Claimed then
               This.Release (Base);
            end if;
         end;
      elsif Op in Floating_Point_Operator
        or else (Op = Op_Mod and then Src_Float)
      then
         declare
            F_Op     : constant String :=
                         (if Op = Op_Mod then "frem" else Op_Name);
            R_1, R_2 : Register_Index;
            C_1, C_2 : Boolean;
         begin
            Materialise_Pair (Src_1_Op, R_1, C_1);
            Materialise_Pair (Src_2_Op, R_2, C_2);
            This.Put_Instruction
              (F_Op, Dst_Image,
               Register_Image (R_1), Register_Image (R_2));
            if C_2 then
               This.Release_Pair (R_2);
            end if;
            if C_1 then
               This.Release_Pair (R_1);
            end if;
         end;
      elsif Op in Compare_Operator and then Src_Float then
         --  fcmp yields -1/0/1 in a single register; feql yields 1/0.
         --  feql is used for equality so that NaN /= NaN holds (fcmp
         --  maps unordered to 0, which would read as equal).
         declare
            R_1, R_2 : Register_Index;
            C_1, C_2 : Boolean;
         begin
            Materialise_Pair (Src_1_Op, R_1, C_1);
            Materialise_Pair (Src_2_Op, R_2, C_2);
            if Op in Op_EQ | Op_NE then
               This.Put_Instruction
                 ("feql", Dst_Image,
                  Register_Image (R_1), Register_Image (R_2));
               if Op = Op_NE then
                  This.Put_Instruction ("zsz", Dst_Image, Dst_Image, "1");
               end if;
            else
               This.Put_Instruction
                 ("fcmp", Dst_Image,
                  Register_Image (R_1), Register_Image (R_2));
               This.Put_Instruction (Op_Name, Dst_Image, Dst_Image, "1");
            end if;
            if C_2 then
               This.Release_Pair (R_2);
            end if;
            if C_1 then
               This.Release_Pair (R_1);
            end if;
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

      Release_Guards;
   end Transfer;

end Tagatha.Arch.Aqua;
