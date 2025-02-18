with Ada.Text_IO;

with Tagatha.Code.Improvements;

package body Tagatha.Code is

   type Trace_Category is (P_Code, Transfers, Optimize);

   Trace_Enabled : array (Trace_Category) of Boolean := [others => False];

   procedure Default_Trace_Proc (Message : String);

   Trace_Proc         : Trace_Callback := Default_Trace_Proc'Access;

   procedure Trace
     (Category : Trace_Category;
      Message  : String);

   function Get_Content
     (A, B, C : Operand_Record := No_Operand)
      return Operand_Content;

   function Get_Content
     (Src_1, Src_2, Dst : Operand_Record;
      Op                : Operator)
      return Operand_Content;

   procedure Create_Transfers
     (This    : in out Instance'Class;
      Routine : in out Routine_Record);

   procedure Update
     (Routine : in out Routine_Record;
      From    : Instruction_Record);

   function To_Transfer
     (Src_1, Src_2 : Operand_Record;
      Op           : Operator;
      T            : Operand_Record)
      return Instruction_Record;

   subtype Constant_Operand_Record is Operand_Record (Constant_Operand);

   function Evaluate
     (Left, Right : Constant_Operand_Record;
      Op          : Binary_Operator)
      return Constant_Operand_Record;

   function Evaluate
     (Right : Constant_Operand_Record;
      Op    : Unary_Operator)
      return Constant_Operand_Record;

   procedure Improve
     (Routine : in out Routine_Record);

   ---------------
   -- Add_Local --
   ---------------

   function Add_Local
     (This : in out Instance)
      return Local_Index
   is
      Routine : Routine_Record renames
                  This.Routine_List (This.Active_Routine);
   begin
      Routine.Last_Loc_Current := Routine.Last_Loc_Current + 1;
      Routine.Last_Loc := Local_Count'Max (Routine.Last_Loc,
                                           Routine.Last_Loc_Current);
      return Routine.Last_Loc_Current;
   end Add_Local;

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Instance'Class;
      Item : Instruction_Record)
   is
      Routine : Routine_Record renames This.Routine_List (This.Active_Routine);
      Instr   : constant Instruction_Record :=
                  (Item with delta Labels => Routine.Set_Labels);
   begin
      Routine.Stack_Code.Append (Instr);
      Update (Routine, Instr);
      Routine.Set_Labels.Clear;
      Trace (P_Code, Instr'Image);
   end Append;

   ----------------------
   -- Append_Data_List --
   ----------------------

   procedure Append_Data_List
     (Data_List : in out Labeled_Data_Lists.List;
      Bits      : Bit_Count;
      List      : Data_Lists.List)
   is
   begin
      if Data_List.Is_Empty then
         Data_List.Append
           (Labeled_Data'
              (Tagatha.Names.To_Name (""), Bits, []));
      end if;

      declare
         Last : Labeled_Data renames Data_List (Data_List.Last);
      begin
         if Last.Bits = 0 or else Last.Data.Is_Empty then
            Last.Bits := Bits;
         end if;
      end;

      if Data_List.Last_Element.Bits /= Bits then
         Data_List.Append
           (Labeled_Data'
              (Tagatha.Names.To_Name (""), Bits, []));
      end if;

      declare
         Item : Data_Lists.List renames
                  Data_List (Data_List.Last).Data;
      begin
         for Element of List loop
            Item.Append (Element);
         end loop;
      end;

   end Append_Data_List;

   -----------------
   -- Begin_Block --
   -----------------

   procedure Begin_Block
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'
           (Block, [], This.Line, This.Column, True));
   end Begin_Block;

   -------------------
   -- Begin_Routine --
   -------------------

   procedure Begin_Routine
     (This    : in out Instance;
      Name    : String;
      Options : Routine_Options'Class := Default_Options)
   is
   begin
      This.Routine_List.Append
        (Routine_Record'
           (Tagatha.Names.To_Name (Name),
            Options  => Routine_Options (Options),
            Last_Arg => Options.Arg_Count,
            others   => <>));
      This.Active_Routine := This.Routine_List.Last;
   end Begin_Routine;

   ------------
   -- Branch --
   ------------

   procedure Branch
     (This        : in out Instance;
      Condition   : Branch_Condition;
      Destination : Label)
   is
      Forward : Boolean;
   begin
      This.Branch_To_Label (Destination, Forward);
      This.Append
        (Instruction_Record'
           (Class     => Branch,
            Labels    => [],
            Line      => This.Line,
            Column    => This.Column,
            Branch_Op => No_Operand,
            Condition => Condition,
            Branch_To => Destination,
            Forward   => Forward));
   end Branch;

   ------------
   -- Branch --
   ------------

   procedure Branch
     (This        : in out Instance;
      Destination : Label)
   is
   begin
      This.Branch (Always, Destination);
   end Branch;

   ---------------------
   -- Branch_To_Label --
   ---------------------

   procedure Branch_To_Label
     (This    : in out Instance'Class;
      L       : Label;
      Forward : out Boolean)
   is
   begin
      while This.Label_Vector.Last_Index <= L loop
         This.Label_Vector.Append
           (Label_Record'
              (others => <>));
      end loop;
      declare
         Rec : Label_Record renames This.Label_Vector (L);
      begin
         Forward := not Rec.Placed;
         Rec.Referenced := True;
      end;
   end Branch_To_Label;

   ----------
   -- Call --
   ----------

   procedure Call
     (This           : in out Instance;
      Name           : String;
      Argument_Count : Natural;
      Result_Count   : Natural)
   is
   begin
      This.Append
        (Instruction_Record'
           (Class          => Call,
            Labels         => [],
            Line           => This.Line,
            Column         => This.Column,
            Name           =>
              Name_Operand (Name, General_Content, False, False),
            Argument_Count => Argument_Count,
            Result_Count   => Result_Count,
            Actuals        => [],
            Is_Subroutine  => True,
            Is_Indirect    => False));
   end Call;

   ----------------------
   -- Create_Transfers --
   ----------------------

   procedure Create_Transfers
     (This    : in out Instance'Class;
      Routine : in out Routine_Record)
   is
      package Operand_Stacks is
        new Ada.Containers.Doubly_Linked_Lists (Operand_Record);

      package Saved_Stacks is
        new Ada.Containers.Doubly_Linked_Lists
          (Operand_Stacks.List, Operand_Stacks."=");

      Operand_Stack : Operand_Stacks.List;
      Saved_Operand_Stacks : Saved_Stacks.List;
      New_Code : Instruction_Vectors.Vector;
      Labels : Label_Lists.List;

      procedure Append (Instruction : Instruction_Record);

      function Pop_Operand return Operand_Record;
      procedure Push_Operand (Operand : Operand_Record);

      ------------
      -- Append --
      ------------

      procedure Append (Instruction : Instruction_Record) is
         New_Labels : Label_Lists.List := Labels;
      begin
         for L of Instruction.Labels loop
            New_Labels.Append (L);
         end loop;
         New_Code.Append
           ((Instruction with delta Labels => New_Labels));
         Labels.Clear;
      end Append;

      -----------------
      -- Pop_Operand --
      -----------------

      function Pop_Operand return Operand_Record is
      begin
         return Top : constant Operand_Record := Operand_Stack.Last_Element do
            Operand_Stack.Delete_Last;
         end return;
      end Pop_Operand;

      ------------------
      -- Push_Operand --
      ------------------

      procedure Push_Operand (Operand : Operand_Record) is
      begin
         Operand_Stack.Append (Operand);
      end Push_Operand;

   begin
      for Instruction of Routine.Stack_Code loop

         if Trace_Enabled (Transfers) then
            for Operand of Operand_Stack loop
               Trace (Transfers, "    " & Operand'Image);
            end loop;
            Trace (Transfers, Instruction'Image);
            Trace (Transfers, "---------------------");
         end if;

         case Instruction.Class is
            when Block =>
               if Instruction.Begin_Block then
                  Saved_Operand_Stacks.Append (Operand_Stack);
               else
                  Operand_Stack := Saved_Operand_Stacks.Last_Element;
                  Saved_Operand_Stacks.Delete_Last;
               end if;
            when Branch =>
               if Instruction.Condition /= Always then
                  Append ((Instruction with delta Branch_Op => Pop_Operand));
               else
                  Append (Instruction);
               end if;
            when Call =>
               declare
                  Name    : Operand_Record := Instruction.Name;
                  Actuals : Operand_Lists.List;
               begin
                  if Instruction.Is_Indirect then
                     Name := Pop_Operand;
                  end if;

                  for I in 1 .. Instruction.Argument_Count loop
                     Actuals.Insert (Operand_Lists.No_Element,
                                     Pop_Operand);
                  end loop;

                  Append ((Instruction with delta
                            Actuals => Actuals, Name => Name));
               end;

            when Control =>
               case Instruction.Control_Class is
                  when Exit_Routine | Fail_Routine | Retry_Routine =>
                     Append (Instruction);
                  when Raise_Exception =>
                     Append ((Instruction with delta
                               Control_Op => Pop_Operand));
               end case;

            when Operate =>
               declare
                  Offset : constant Operand_Record :=
                             (if Instruction.Op = Op_Store
                              then Pop_Operand
                              else No_Operand);
                  Src_2 : constant Operand_Record := Pop_Operand;
                  Src_1 : constant Operand_Record :=
                            (if Instruction.Op in Unary_Operator
                             then No_Operand
                             else Pop_Operand);
                  T      : constant Operand_Record :=
                             (if Instruction.Op = Op_Store
                              then No_Operand
                              else (Class       => Temporary_Operand,
                                    Dereference => False,
                                    Offset      => 0,
                                    Content     =>
                                      Get_Content (Src_1, Src_2,
                                       No_Operand, Instruction.Op),
                                    Temp        => This.Next_Temporary));
               begin
                  if Instruction.Op = Op_Store then
                     if Src_2.Class = Stack_Operand
                       and then Src_2.Reference
                       and then Offset.Word = 0
                     then
                        declare
                           Move : constant Instruction_Record :=
                                    Instruction_Record'
                                      (Class     => Transfer,
                                       Labels    => Instruction.Labels,
                                       Line      => Instruction.Line,
                                       Column    => Instruction.Column,
                                       Src_1     => No_Operand,
                                       Src_2     => Src_1,
                                       T_Op      => Op_Identity,
                                       Dst       =>
                                         (Src_2 with delta
                                            Reference => False));
                        begin
                           Append (Move);
                        end;
                     else
                        declare
                           Store : constant Instruction_Record :=
                                     Instruction_Record'
                                       (Class     => Transfer,
                                        Labels    => Instruction.Labels,
                                        Line      => Instruction.Line,
                                        Column    => Instruction.Column,
                                        Src_1     => Src_1,
                                        Src_2     => Offset,
                                        T_Op      => Op_Store,
                                        Dst       => Src_2);
                        begin
                           Trace (Transfers, Store'Image);
                           Append (Store);
                        end;
                     end if;
                  else
                     declare
                        Transfer : constant Instruction_Record :=
                                     To_Transfer
                                       (Src_1, Src_2, Instruction.Op, T);
                     begin
                        Append ((Transfer with delta
                                  Labels => Instruction.Labels,
                                Line   => Instruction.Line,
                                Column => Instruction.Column));
                     end;
                     Push_Operand (T);
                  end if;
               end;
            when Push =>
               for Label of Instruction.Labels loop
                  Labels.Append (Label);
               end loop;
               if Instruction.Operand.Class = Stack_Operand
                 and then Instruction.Operand.Stack_Op = Return_Operand
               then
                  declare
                     T     : constant Operand_Record :=
                               (Class       => Temporary_Operand,
                                Dereference => False,
                                Offset      => 0,
                                Content     => Instruction.Operand.Content,
                                Temp        => This.Next_Temporary);
                  begin
                     Append
                       (Instruction_Record'
                          (Transfer, Instruction.Labels,
                           Instruction.Line, Instruction.Column,
                           No_Operand, Instruction.Operand, Op_Identity,
                           T));

                     Push_Operand (T);
                  end;
               else
                  Push_Operand (Instruction.Operand);
               end if;
            when Pop =>
               Append
                 (Instruction_Record'
                    (Transfer, Instruction.Labels,
                     Instruction.Line, Instruction.Column,
                     No_Operand, Pop_Operand, Op_Identity,
                     Instruction.Operand));
            when Stack =>
               for Label of Instruction.Labels loop
                  Labels.Append (Label);
               end loop;

               case Instruction.Stack_Op is
                  when Drop =>
                     declare
                        Op : constant Operand_Record := Pop_Operand;
                     begin
                        pragma Unreferenced (Op);
                     end;

                  when Duplicate =>
                     Push_Operand (Operand_Stack.Last_Element);

                  when Pop =>
                     declare
                        Dst : constant Operand_Record := Pop_Operand;
                        Src : constant Operand_Record := Pop_Operand;
                     begin
                        Append
                          (Instruction_Record'
                             (Transfer, Instruction.Labels,
                              Instruction.Line, Instruction.Column,
                              No_Operand, Src, Op_Identity, Dst));
                     end;

                  when Swap =>
                     declare
                        Op_1 : constant Operand_Record := Pop_Operand;
                        Op_2 : constant Operand_Record := Pop_Operand;
                     begin
                        Push_Operand (Op_1);
                        Push_Operand (Op_2);
                     end;

               end case;

            when Transfer =>
               Append (Instruction);
         end case;
      end loop;
      Routine.Transfer_Code := New_Code;

      if Trace_Enabled (Transfers) then
         Trace_Proc ("--- TRANSFERS");
         for T of Routine.Transfer_Code loop
            Trace_Proc (T'Image);
         end loop;
      end if;
   end Create_Transfers;

   ----------
   -- Data --
   ----------

   procedure Data
     (This   : in out Instance;
      Values : Word_8_Array)
   is
      List : Data_Lists.List;
   begin
      for V of Values loop
         List.Append (Word_64_Element (Word_64 (V)));
      end loop;
      for I in 1 .. 4 - Values'Length mod 4 loop
         List.Append (Word_64_Element (0));
      end loop;
      Append_Data_List (This.RO_Data_List, 8, List);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data
     (This   : in out Instance;
      Value  : Int_32)
   is
   begin
      Append_Data_List
        (This.RO_Data_List, 32,
         [Word_64_Element (Conversions.Int_32_To_Word_64 (Value))]);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data
     (This   : in out Instance;
      Label  : String)
   is
   begin
      Append_Data_List
        (This.RO_Data_List, 32,
         [Label_Element (Label)]);
   end Data;

   ----------------
   -- Data_Label --
   ----------------

   procedure Data_Label
     (This : in out Instance;
      Name : String)
   is
   begin
      This.RO_Data_List.Append
        (Labeled_Data'
           (Name => Tagatha.Names.To_Name (Name),
            Bits => 0,
            Data => []));
   end Data_Label;

   -------------------
   -- Data_Label_RW --
   -------------------

   procedure Data_Label_RW
     (This : in out Instance;
      Name : String)
   is
   begin
      This.RW_Data_List.Append
        (Labeled_Data'
           (Name => Tagatha.Names.To_Name (Name),
            Bits => 0,
            Data => []));
   end Data_Label_RW;

   ----------
   -- Data --
   ----------

   procedure Data_RW
     (This   : in out Instance;
      Value  : Int_32)
   is
   begin
      Append_Data_List
        (This.RW_Data_List, 32,
         [Word_64_Element (Conversions.Int_32_To_Word_64 (Value))]);
   end Data_RW;

   ---------------------
   -- Default_Options --
   ---------------------

   function Default_Options return Routine_Options'Class is
   begin
      return Routine_Options'(others => <>);
   end Default_Options;

   ------------------------
   -- Default_Trace_Proc --
   ------------------------

   procedure Default_Trace_Proc (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Default_Trace_Proc;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (This    : in out Instance;
      Content : Operand_Content;
      Offset  : Int_32)
   is
   begin
      This.Push_Constant (Offset);
      This.Operate (Op_Dereference);
   end Dereference;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (This    : in out Instance;
      Offset  : Int_32 := 0)
   is
   begin
      This.Dereference (General_Content, Offset);
   end Dereference;

   ----------
   -- Drop --
   ----------

   procedure Drop
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'(Stack, [], This.Line, This.Column, Drop));
   end Drop;

   ---------------
   -- Duplicate --
   ---------------

   procedure Duplicate
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'(Stack, [], This.Line, This.Column, Duplicate));
   end Duplicate;

   ------------------
   -- Enable_Trace --
   ------------------

   procedure Enable_Trace
     (Enable_P_Code       : Boolean := False;
      Enable_Transfers    : Boolean := False;
      Enable_Improvements : Boolean := False)
   is
   begin
      Trace_Enabled :=
        [P_Code      => Enable_P_Code,
         Transfers   => Enable_Transfers,
         Optimize    => Enable_Improvements];
   end Enable_Trace;

   ---------------
   -- End_Block --
   ---------------

   procedure End_Block
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'
           (Block, [], This.Line, This.Column, False));
   end End_Block;

   -----------------
   -- End_Routine --
   -----------------

   procedure End_Routine
     (This : in out Instance)
   is
      Routine : Routine_Record renames
                  This.Routine_List (This.Active_Routine);
   begin
      This.Create_Transfers (Routine);
      This.Active_Routine := Routine_Lists.No_Element;
   end End_Routine;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Right : Constant_Operand_Record;
      Op    : Unary_Operator)
      return Constant_Operand_Record
   is
      Y : constant Word_64 := Right.Word;
      Z : Word_64;
   begin
      case Op is
         when Op_Identity =>
            Z := Y;
         when Op_Negate =>
            Z := Word_64'Last - Y;
         when Op_Not =>
            Z := (if Y = 0 then 1 else 0);
         when Op_Test =>
            Z := (if Y = 0 then 0 else 1);
      end case;
      return Constant_Operand (Z, Right.Content);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Left, Right : Constant_Operand_Record;
      Op          : Binary_Operator)
      return Constant_Operand_Record
   is
      X : constant Word_64 := Left.Word;
      Y : constant Word_64 := Right.Word;
      Z : Word_64;
   begin
      case Op is
         when Op_Add =>
            Z := X + Y;
         when Op_Subtract =>
            Z := X - Y;
         when Op_Multiply =>
            Z := X * Y;
         when Op_Divide =>
            Z := X / Y;
         when Op_Mod =>
            Z := X mod Y;
         when Floating_Point_Operator =>
            declare
               FX : constant Floating_Point_Constant :=
                      Conversions.Word_64_To_Floating_Point (X);
               FY : constant Floating_Point_Constant :=
                      Conversions.Word_64_To_Floating_Point (Y);
               FZ : constant Floating_Point_Constant :=
                      (case Floating_Point_Operator (Op) is
                          when Op_Fadd => FX + FY,
                          when Op_Fsub => FX - FY,
                          when Op_Fmul => FX * FY,
                          when Op_Fdiv => FX / FY);
            begin
               Z := Conversions.Floating_Point_To_Word_64 (FZ);
            end;
         when Op_And =>
            Z := X and Y;
         when Op_Or =>
            Z := X or Y;
         when Op_Xor =>
            Z := X xor Y;
         when Op_Dereference =>
            raise Program_Error with
              "cannot evalute dereference with constant operands";
         when Op_Store =>
            raise Program_Error with
              "cannot evalute store with constant operands";
         when Op_EQ =>
            Z := Boolean'Pos (X = Y);
         when Op_LT =>
            Z := Boolean'Pos (X < Y);
         when Op_GT =>
            Z := Boolean'Pos (X > Y);
         when Op_LE =>
            Z := Boolean'Pos (X <= Y);
         when Op_GE =>
            Z := Boolean'Pos (X >= Y);
         when Op_NE =>
            Z := Boolean'Pos (X /= Y);
      end case;
      return Constant_Operand
        (Z, Derive_Content (Left.Content, Right.Content));
   end Evaluate;

   -----------------------
   -- Exception_Handler --
   -----------------------

   procedure Exception_Handler
     (This          : in out Instance;
      Start_Label   : String;
      End_Label     : String;
      Handler_Label : String)
   is
   begin
      This.Handlers.Append
        (Exception_Handler_Record'
           (Base    => Tagatha.Names.To_Name (Start_Label),
            Bound   => Tagatha.Names.To_Name (End_Label),
            Handler => Tagatha.Names.To_Name (Handler_Label)));
   end Exception_Handler;

   ------------------
   -- Exit_Routine --
   ------------------

   procedure Exit_Routine
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'
           (Control, [], This.Line, This.Column, Exit_Routine, No_Operand));
   end Exit_Routine;

   ------------------
   -- Fail_Routine --
   ------------------

   procedure Fail_Routine
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'
           (Control, [], This.Line, This.Column, Fail_Routine, No_Operand));
   end Fail_Routine;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (This   : in out Instance;
      Target : in out Tagatha.Arch.Instance'Class)
   is

      procedure Gen_Instruction
        (Routine : Routine_Record;
         Index   : Positive);

      procedure Update_Temporaries
        (Routine : in out Routine_Record;
         Index   : Positive);

      ---------------------
      -- Gen_Instruction --
      ---------------------

      procedure Gen_Instruction
        (Routine : Routine_Record;
         Index   : Positive)
      is

         Instruction : constant Instruction_Record :=
                         Routine.Transfer_Code (Index);

         function Get_Operand
           (From   : Operand_Record;
            Is_Dst : Boolean := False)
            return Tagatha.Arch.Operand_Interface'Class;

         -----------------
         -- Get_Operand --
         -----------------

         function Get_Operand
           (From   : Operand_Record;
            Is_Dst : Boolean := False)
            return Tagatha.Arch.Operand_Interface'Class
         is
         begin
            case From.Class is
               when No_Operand =>
                  return Target.No_Operand;
               when Stack_Operand =>
                  pragma Assert (Is_Dst or else
                                   not From.Reference, Instruction'Image);
                  case From.Stack_Op is
                     when Argument_Operand =>
                        return Target.Argument_Operand
                          (From.Content, Argument_Index (From.Index));
                     when Local_Operand =>
                        return Target.Local_Operand
                          (From.Content, Local_Index (From.Index));
                     when Result_Operand =>
                        return Target.Result_Operand
                          (From.Content, Result_Index (From.Index));
                     when Return_Operand =>
                        return Target.Return_Operand
                          (From.Content, Return_Index (From.Index));
                  end case;
               when Constant_Operand =>
                  return Target.Constant_Operand
                    (From.Content, From.Word);
               when External_Operand =>
                  return Target.Name_Operand
                    (Tagatha.Names.To_String (From.Name),
                     From.Address, From.Imported);
               when Temporary_Operand =>
                  declare
                     Rec         : constant Temporary_Record :=
                                     Routine.Temporaries (From.Temp);
                     First_Write : constant Boolean :=
                                     Is_Dst and then Rec.First_Write = Index;
                     Last_Read   : constant Boolean :=
                                     not Is_Dst and then Rec.Last_Read = Index;
                     Remap       : constant Operand_Record :=
                                     Rec.Remap;
                  begin
                     --  Ada.Text_IO.Put_Line
                     --    ("get-operand: t"
                     --     & Integer'Image (-Integer (From.Temp))
                     --     & "; index" & Index'Image
                     --     & "; first-write" & Rec.First_Write'Image
                     --       & "; last-read" & Rec.Last_Read'Image);
                     if Remap.Class = No_Operand then
                        return Target.Temporary_Operand
                          (From.Temp, From.Content, First_Write, Last_Read);
                     else
                        return Get_Operand (Remap, Is_Dst);
                     end if;
                  end;

            end case;

         end Get_Operand;

      begin

         if False then
            Target.Put_Comment (Instruction'Image);
         end if;

         Target.Set_Source_Location (Instruction.Line, Instruction.Column);
         for Label of Instruction.Labels loop
            This.Put_Label (Target, Label);
         end loop;

         case Instruction.Class is
            when Block =>
               null;
            when Branch =>
               Target.Branch (Get_Operand (Instruction.Branch_Op),
                              Instruction.Condition,
                              Positive (Instruction.Branch_To),
                              Instruction.Forward);
            when Call =>
               if Instruction.Is_Subroutine then
                  declare
                     Actuals : Tagatha.Arch.Operand_Lists.List;
                  begin
                     for Arg of Instruction.Actuals loop
                        Actuals.Append (Get_Operand (Arg));
                     end loop;

                     Target.Call
                       (Destination    => Get_Operand (Instruction.Name),
                        Actuals        => Actuals,
                        Result_Count   => Instruction.Result_Count);
                  end;
               else
                  Target.Jump (Get_Operand (Instruction.Name));
               end if;
            when Control =>
               case Instruction.Control_Class is
                  when Exit_Routine =>
                     Target.Exit_Routine;
                  when Fail_Routine =>
                     Target.Fail_Routine;
                  when Raise_Exception =>
                     Target.Raise_Exception
                       (Get_Operand (Instruction.Control_Op));
                  when Retry_Routine =>
                     Target.Retry
                       (Get_Operand (Instruction.Control_Op).Image);
               end case;
            when Operate =>
               null;
            when Push =>
               Ada.Text_IO.Put_Line (Instruction'Image);
            when Pop =>
               null;
            when Stack =>
               null;
            when Transfer =>
               declare
                  Src_1 : constant Tagatha.Arch.Operand_Interface'Class :=
                            Get_Operand (Instruction.Src_1);
                  Src_2 : constant Tagatha.Arch.Operand_Interface'Class :=
                            Get_Operand (Instruction.Src_2);
                  Dst   : constant Tagatha.Arch.Operand_Interface'Class :=
                            Get_Operand (Instruction.Dst, True);
               begin
                  Target.Transfer (Dst, Src_1, Src_2, Instruction.T_Op);
               end;
         end case;
      end Gen_Instruction;

      ------------------------
      -- Update_Temporaries --
      ------------------------

      procedure Update_Temporaries
        (Routine : in out Routine_Record;
         Index   : Positive)
      is

         procedure Update_Operand
           (Operand : Operand_Record;
            Is_Dst  : Boolean := False);

         --------------------
         -- Update_Operand --
         --------------------

         procedure Update_Operand
           (Operand : Operand_Record;
            Is_Dst  : Boolean := False)
         is
         begin
            case Operand.Class is
               when No_Operand =>
                  null;
               when Stack_Operand =>
                  null;
               when Constant_Operand =>
                  null;
               when External_Operand =>
                  null;
               when Temporary_Operand =>
                  while Routine.Temporaries.Last_Index <= Operand.Temp loop
                     Routine.Temporaries.Append
                       (Temporary_Record'(others => <>));
                  end loop;

                  declare
                     Rec : Temporary_Record renames
                             Routine.Temporaries (Operand.Temp);
                  begin
                     if Is_Dst then
                        if Rec.First_Write = 0 then
                           Rec.First_Write := Index;
                        end if;
                     else
                        Rec.Last_Read := Index;
                     end if;
                  end;
            end case;

         end Update_Operand;

         Instruction : constant Instruction_Record :=
                         Routine.Transfer_Code (Index);
      begin
         case Instruction.Class is
            when Block =>
               null;
            when Branch =>
               Update_Operand (Instruction.Branch_Op);
            when Call =>
               for Actual of Instruction.Actuals loop
                  Update_Operand (Actual);
               end loop;
            when Control =>
               null;
            when Operate =>
               null;
            when Push =>
               null;
            when Pop =>
               null;
            when Stack =>
               null;
            when Transfer =>
               Update_Operand (Instruction.Src_1);
               Update_Operand (Instruction.Src_2);
               Update_Operand (Instruction.Dst, True);
         end case;
      end Update_Temporaries;

   begin
      for Routine of This.Routine_List loop

         Improve (Routine);

         if Trace_Enabled (Optimize) then
            Trace_Proc ("--- IMPROVEMENTS");
            for T of Routine.Transfer_Code loop
               Trace_Proc (T'Image);
            end loop;
            Trace_Proc ("--- DONE");

         end if;

         Target.Begin_Routine
           (Tagatha.Names.To_String (Routine.Name),
            Routine.Last_Arg, Routine.Last_Res,
            Routine.Last_Loc, Routine.Options.Linkage);

         for Index in 1 .. Routine.Transfer_Code.Last_Index loop
            Update_Temporaries (Routine, Index);
         end loop;

         for Index in 1 .. Routine.Transfer_Code.Last_Index loop
            Gen_Instruction (Routine, Index);
         end loop;

         for Label of Routine.Set_Labels loop
            This.Put_Label (Target, Label);
         end loop;

         Target.End_Routine;
      end loop;

      for Data of This.RO_Data_List loop
         Target.Begin_Data
           (Tagatha.Names.To_String (Data.Name),
            Natural (Data.Bits),
            Read_Write => False);
         for Datum of Data.Data loop
            case Datum.Class is
               when Word_64_Element =>
                  Target.Datum (Datum.Value);
               when Label_Element =>
                  Target.Label_Datum (Tagatha.Names.To_String (Datum.Name));
            end case;
         end loop;
         Target.End_Data;
      end loop;

      for Data of This.RW_Data_List loop
         Target.Begin_Data
           (Tagatha.Names.To_String (Data.Name),
            Natural (Data.Bits),
            Read_Write => True);
         for Datum of Data.Data loop
            case Datum.Class is
               when Word_64_Element =>
                  Target.Datum (Datum.Value);
               when Label_Element =>
                  Target.Label_Datum (Tagatha.Names.To_String (Datum.Name));
            end case;
         end loop;
         Target.End_Data;
      end loop;

      for Note of This.Notes loop
         Target.Note
           (Name => Tagatha.Names.To_String (Note.Name),
            Tag  => Note.Tag,
            Text => Tagatha.Names.To_String (Note.Description));
      end loop;

      for Handler of This.Handlers loop
         Target.Note
           (Name => "exception_handler",
            Tag  => 1,
            Text =>
              Tagatha.Names.To_String (Handler.Base)
            & ","
            & Tagatha.Names.To_String (Handler.Bound)
            & ","
            & Tagatha.Names.To_String (Handler.Handler));
      end loop;

   end Generate;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
     (A, B, C : Operand_Record := No_Operand)
      return Operand_Content
   is
      Result : Operand_Content := Operand_Content'First;

      procedure Update (X : Operand_Content);

      ------------
      -- Update --
      ------------

      procedure Update (X : Operand_Content) is
      begin
         Result := Operand_Content'Max (Result, X);
      end Update;

   begin
      Update (A.Content);
      Update (B.Content);
      Update (C.Content);
      return Result;
   end Get_Content;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
     (Src_1, Src_2, Dst : Operand_Record;
      Op                : Operator)
      return Operand_Content
   is
      Default : constant Operand_Content :=
                  Get_Content (Src_1, Src_2, Dst);
   begin
      case Op is
         when Op_Test =>
            return General_Content;
         when Compare_Operator =>
            return General_Content;
         when others =>
            return Default;
      end case;
   end Get_Content;

   -------------
   -- Improve --
   -------------

   procedure Improve
     (Routine : in out Routine_Record)
   is
      Changed : Boolean := True;
   begin
      while Changed loop
         declare
            use Instruction_Vectors;
            Code    : Vector renames Routine.Transfer_Code;
            Index   : Positive := Code.First_Index;
         begin
            Changed := False;
            while Index < Code.Last_Index loop
               declare

                  Applied : Boolean := False;

                  procedure Try
                    (Improvement : Improvements.Improver_Interface'Class;
                     Stop        : out Boolean);

                  ---------
                  -- Try --
                  ---------

                  procedure Try
                    (Improvement : Improvements.Improver_Interface'Class;
                     Stop        : out Boolean)
                  is
                     Instr : constant Instruction_Record :=
                               Code (Index);
                     Next  : constant Instruction_Record :=
                               Code (Index + 1);

                  begin
                     if Improvement.Test (Instr, Next) then

                        if Trace_Enabled (Optimize) then
                           Trace_Proc
                             ("applying: " & Improvement.Name);
                           Trace_Proc
                             ("  current: " & Instr'Image);
                           Trace_Proc
                             ("  next:    " & Next'Image);
                        end if;

                        declare
                           Changes : constant Code_Change_Lists.List :=
                                       Improvement.Fix (Instr, Next);
                        begin
                           for Change of Changes loop
                              case Change.Operation is
                                 when Replace_Instruction =>
                                    declare
                                       New_Instr : Instruction_Record :=
                                                     Change.New_Instruction
                                                       .Element;
                                    begin
                                       New_Instr.Labels.Clear;
                                       for I in
                                         Change.First .. Change.Last
                                       loop
                                          for Label of
                                            Code (Index + I).Labels
                                          loop
                                             New_Instr.Labels.Append (Label);
                                          end loop;
                                       end loop;
                                       Code.Replace_Element (Index, New_Instr);
                                       if Change.Last > Change.First then
                                          Code.Delete
                                            (Index + 1,
                                             Ada.Containers.Count_Type
                                               (Change.Last - Change.First));
                                       end if;
                                       if Trace_Enabled (Optimize) then
                                          Trace_Proc
                                            ("  new:     " & New_Instr'Image);
                                       end if;
                                    end;

                                 when Remap_Temporary =>
                                    if Trace_Enabled (Optimize) then
                                       Trace_Proc
                                         ("remap" & Change.Temp'Image
                                          & " to " & Change.Map_To'Image);
                                    end if;
                                    while Routine.Temporaries.Last_Index
                                      < Change.Temp
                                    loop
                                       Routine.Temporaries.Append
                                         (Temporary_Record'(others => <>));
                                    end loop;
                                    Routine.Temporaries (Change.Temp)
                                      .Remap := Change.Map_To;
                              end case;
                           end loop;
                           Stop := True;
                           Applied := True;

                        end;
                     else
                        Stop := False;
                     end if;
                  end Try;

               begin
                  Improvements.Iterate_Improvements (Try'Access);
                  if Applied then
                     Changed := True;
                  else
                     Index := Index + 1;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Improve;

   -------------------
   -- Indirect_Call --
   -------------------

   procedure Indirect_Call
     (This           : in out Instance;
      Argument_Count : Natural;
      Result_Count   : Natural)
   is
   begin
      This.Append
        (Instruction_Record'
           (Call, [], This.Line, This.Column, No_Operand,
            Argument_Count, Result_Count, [],
            Is_Subroutine => True,
            Is_Indirect   => True));
   end Indirect_Call;

   ----------
   -- Jump --
   ----------

   procedure Jump
     (This           : in out Instance;
      Name           : String)
   is
   begin
      This.Append
        (Instruction_Record'
           (Call, [], This.Line, This.Column,
            Name_Operand (Name, General_Content, False, False),
            0, 0, [],
            Is_Subroutine => False,
            Is_Indirect   => False));
   end Jump;

   -----------------
   -- Named_Label --
   -----------------

   function Named_Label
     (This : in out Instance;
      Name : String)
      return Label
   is
   begin
      return L : constant Label := This.Next_Label do
         This.Named_Labels.Insert (L, Name);
      end return;
   end Named_Label;

   ----------------
   -- Next_Label --
   ----------------

   function Next_Label
     (This : in out Instance)
      return Label
   is
   begin
      This.Last_Label := This.Last_Label + 1;
      return This.Last_Label;
   end Next_Label;

   --------------------
   -- Next_Temporary --
   --------------------

   function Next_Temporary
     (This : in out Instance'Class)
      return Temporary_Index
   is
   begin
      This.Last_Temp := This.Last_Temp + 1;
      return This.Last_Temp;
   end Next_Temporary;

   ----------
   -- Note --
   ----------

   procedure Note
     (This : in out Instance;
      Name : String;
      Tag  : Word_32;
      Text : String)
   is
      Rec : constant Note_Record := Note_Record'
        (Name        => Tagatha.Names.To_Name (Name),
         Tag         => Tag,
         Description => Tagatha.Names.To_Name (Text));
   begin
      This.Notes.Append (Rec);
   end Note;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (This : in out Instance;
      Op   : Operator)
   is
   begin
      This.Append
        (Instruction_Record'
           (Operate, [], This.Line, This.Column, Op));
   end Operate;

   -----------------
   -- Place_Label --
   -----------------

   procedure Place_Label
     (This : in out Instance'Class;
      L    : Label)
   is
   begin
      while This.Label_Vector.Last_Index <= L loop
         This.Label_Vector.Append
           (Label_Record'
              (others => <>));
      end loop;
      declare
         Rec : Label_Record renames This.Label_Vector (L);
      begin
         Rec.Placed := True;
      end;
   end Place_Label;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (This : in out Instance'Class;
      Item : Operand_Record)
   is
   begin
      This.Append
        (Instruction_Record'
           (Pop, [], This.Line, This.Column, Item));
   end Pop;

   procedure Pop
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'(Stack, [], This.Line, This.Column, Pop));
   end Pop;

   ------------------
   -- Pop_Argument --
   ------------------

   procedure Pop_Argument
     (This  : in out Instance;
      Index   : Argument_Index;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Pop (Argument_Operand (Index, Content));
   end Pop_Argument;

   ------------------
   -- Pop_Indirect --
   ------------------

   procedure Pop_Indirect
     (This    : in out Instance;
      Content : Operand_Content := General_Content;
      Offset  : Int_32 := 0)
   is
   begin
      This.Push_Constant (Offset);
      This.Operate (Op_Store);
   end Pop_Indirect;

   ---------------
   -- Pop_Local --
   ---------------

   procedure Pop_Local
     (This  : in out Instance;
      Index   : Local_Index;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Pop (Local_Operand (Index, Content));
   end Pop_Local;

   --------------
   -- Pop_Name --
   --------------

   procedure Pop_Name
     (This    : in out Instance;
      Name    : String;
      Extern  : Boolean;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Pop (Name_Operand (Name, Content, False, Extern));
   end Pop_Name;

   ----------------
   -- Pop_Result --
   ----------------

   procedure Pop_Result
     (This  : in out Instance;
      Index   : Result_Index;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Pop (Result_Operand (Index, Content));
   end Pop_Result;

   ----------
   -- Push --
   ----------

   procedure Push
     (This : in out Instance'Class;
      Item : Operand_Record)
   is
   begin
      This.Append
        (Instruction_Record'
           (Push, [], This.Line, This.Column, Item));
   end Push;

   -------------------
   -- Push_Argument --
   -------------------

   procedure Push_Argument
     (This    : in out Instance;
      Index   : Argument_Index;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Push (Argument_Operand (Index, Content));
   end Push_Argument;

   -------------------
   -- Push_Constant --
   -------------------

   procedure Push_Constant
     (This  : in out Instance;
      Value : Int_32)
   is
   begin
      This.Push (Constant_Operand (Value));
   end Push_Constant;

   -------------------
   -- Push_Constant --
   -------------------

   procedure Push_Constant
     (This  : in out Instance;
      Value : Floating_Point_Constant)
   is
   begin
      This.Push (Constant_Operand (Value));
   end Push_Constant;

   -------------------
   -- Push_Constant --
   -------------------

   procedure Push_Constant
     (This    : in out Instance;
      Value   : Word_64;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Push (Constant_Operand (Value, Content));
   end Push_Constant;

   ----------------
   -- Push_Local --
   ----------------

   procedure Push_Local
     (This      : in out Instance;
      Index     : Local_Index;
      Content   : Operand_Content := General_Content;
      Reference : Boolean := False)
   is
   begin
      This.Push (Local_Operand (Index, Content, Reference));
   end Push_Local;

   ---------------
   -- Push_Name --
   ---------------

   procedure Push_Name
     (This    : in out Instance;
      Name    : String;
      Extern  : Boolean;
      Content : Operand_Content := General_Content;
      Address : Boolean := False)
   is
   begin
      This.Push (Name_Operand (Name, Content, Address, Extern));
   end Push_Name;

   -----------------
   -- Push_Return --
   -----------------

   procedure Push_Return
     (This  : in out Instance;
      Index   : Return_Index;
      Content : Operand_Content := General_Content)
   is
   begin
      This.Push (Return_Operand (Index, Content));
   end Push_Return;

   ---------------------------
   -- Put_Instruction_Image --
   ---------------------------

   procedure Put_Instruction_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Instruction_Record)
   is

      function Image (Op : Operator) return String
      is (case Op is
             when Op_Identity           => "id",
             when Op_Negate             => "-",
             when Op_Not                => "~",
             when Op_Test               => "tst",
             when Op_Add | Op_Fadd      => "+",
             when Op_Subtract | Op_Fsub => "-",
             when Op_Multiply | Op_Fmul => "*",
             when Op_Divide | Op_Fdiv   => "/",
             when Op_Mod                => "mod",
             when Op_And                => "&",
             when Op_Or                 => "|",
             when Op_Xor                => "xor",
             when Op_Dereference        => "^",
             when Op_Store              => "!",
             when Op_EQ                 => "=",
             when Op_NE                 => "/=",
             when Op_LT                 => "<",
             when Op_LE                 => "<=",
             when Op_GT                 => ">",
             when Op_GE                 => ">=");

      Line_Img : constant String := Value.Line'Image;
      Col_Img  : constant String := Value.Column'Image;

   begin
      for Label of Value.Labels loop
         Output.Put (Label'Image & ":");
      end loop;
      Output.Put ("[" & Line_Img (2 .. Line_Img'Last) & ":"
                  & Col_Img (2 .. Col_Img'Last) & "] ");
      case Value.Class is
         when Block =>
            Output.Put (if Value.Begin_Block then "BEGIN" else "END");
         when Branch =>
            Output.Put ("B" & Value.Condition'Image
                        & " " & Value.Branch_Op'Image
                        & Value.Branch_To'Image);
         when Call =>
            Output.Put
              ("CALL " & Value.Name'Image
               & Value.Argument_Count'Image
               & Value.Result_Count'Image);
         when Control =>
            case Value.Control_Class is
               when Exit_Routine =>
                  Output.Put ("EXIT");
               when Fail_Routine =>
                  Output.Put ("FAIL");
               when Raise_Exception =>
                  Output.Put ("RAISE " & Value.Control_Op'Image);
               when Retry_Routine =>
                  Output.Put ("RETRY");
            end case;
         when Operate =>
            Output.Put (Value.Op'Image);
         when Push =>
            Output.Put ("PUSH " & Value.Operand'Image);
         when Pop =>
            Output.Put ("POP " & Value.Operand'Image);
         when Stack =>
            Output.Put (Value.Stack_Op'Image);
         when Transfer =>
            if Value.T_Op = Op_Identity then
               Output.Put (Value.Dst'Image & " <- " & Value.Src_2'Image);
            elsif Value.T_Op = Op_Store then
               if Value.Src_2.Class = No_Operand
                 or else (Value.Src_2.Class = Constant_Operand
                          and then Value.Src_2.Word = 0)
               then
                  Output.Put ("[" & Value.Dst'Image & "] <- "
                              & Value.Src_1'Image);
               else
                  Output.Put ("[" & Value.Dst'Image & "] <- "
                              & Value.Src_1'Image
                              & " + " & Value.Src_2'Image);
               end if;
            elsif Value.T_Op in Unary_Operator then
               Output.Put (Value.Dst'Image & " <- " & Image (Value.T_Op)
                           & " " & Value.Src_2'Image);
            elsif Value.T_Op = Op_Dereference then
               Output.Put (Value.Dst'Image & " <- ");
               if Value.Src_2.Class = Constant_Operand
                 and then Value.Src_2.Word = 0
               then
                  Output.Put ("(" & Value.Src_1'Image & ")");
               else
                  Output.Put
                    (Value.Src_2'Image & "(" & Value.Src_1'Image & ")");
               end if;
            else
               Output.Put (Value.Dst'Image & " <- "
                           & Value.Src_1'Image
                           & " "
                           & Image (Value.T_Op)
                           & " "
                           & Value.Src_2'Image);
            end if;
      end case;
   end Put_Instruction_Image;

   ---------------
   -- Put_Label --
   ---------------

   procedure Put_Label
     (This   : in out Instance;
      Target : in out Tagatha.Arch.Instance'Class;
      L      : Label)
   is
      Position : constant Named_Label_Maps.Cursor :=
                   This.Named_Labels.Find (L);
   begin
      if Named_Label_Maps.Has_Element (Position) then
         Target.Name_Label (This.Named_Labels (Position));
      else
         Target.Local_Label (Positive (L));
      end if;
   end Put_Label;

   -----------------------
   -- Put_Operand_Image --
   -----------------------

   procedure Put_Operand_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Operand_Record)
   is
      Suffix : constant String :=
                 (case Value.Content is
                     when General_Content => "",
                     when Floating_Point_Content => ".f");
   begin
      case Value.Class is
         when No_Operand =>
            Output.Put ("<>");
         when Constant_Operand =>
            Output.Put (Value.Word'Image & Suffix);
         when External_Operand =>
            Output.Put (Tagatha.Names.To_String (Value.Name) & Suffix);
         when Stack_Operand =>
            declare
               Index : constant String :=
                         Integer'Image (-Integer (Value.Index));
               Ref    : constant String :=
                          (if Value.Reference
                           then "&" else "");
               Prefix : constant String :=
                          (case Value.Stack_Op is
                              when Argument_Operand =>
                                "arg",
                              when Local_Operand    =>
                                "loc",
                              when Result_Operand   =>
                                "res",
                              when Return_Operand   =>
                                "ret");
            begin
               Output.Put (Ref & Prefix & Index & Suffix);
            end;
         when Temporary_Operand =>
            case Value.Content is
               when General_Content =>
                  Output.Put ("t" & Integer'Image (-Integer (Value.Temp)));
               when Floating_Point_Content =>
                  Output.Put ("f" & Integer'Image (-Integer (Value.Temp)));
            end case;
      end case;
   end Put_Operand_Image;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'
           (Control, [], This.Line, This.Column, Raise_Exception, No_Operand));
   end Raise_Exception;

   ------------------
   -- Remove_Local --
   ------------------

   procedure Remove_Local
     (This : in out Instance)
   is
      Routine : Routine_Record renames
                  This.Routine_List (This.Active_Routine);
   begin
      Routine.Last_Loc_Current := Routine.Last_Loc_Current - 1;
   end Remove_Local;

   -------------------
   -- Retry_Routine --
   -------------------

   procedure Retry_Routine
     (This : in out Instance)
   is
      Retry_Label : constant String :=
                      Tagatha.Names.To_String
                        (This.Routine_List (This.Active_Routine).Name)
                      & "$retry";
   begin
      This.Append
        (Instruction_Record'
           (Control, [], This.Line, This.Column, Retry_Routine,
            Name_Operand (Retry_Label, General_Content, False, False)));
   end Retry_Routine;

   ----------
   -- Save --
   ----------

   procedure Save
     (This : Instance;
      Path : String)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for Routine of This.Routine_List loop
         Put_Line (File,
                   Tagatha.Names.To_String (Routine.Name) & ":");
         for Code of Routine.Transfer_Code loop
            Put_Line (File, "   " & Code'Image);
         end loop;
      end loop;
      Close (File);
   end Save;

   ------------------------
   -- Set_Argument_Count --
   ------------------------

   function Set_Argument_Count
     (This  : Routine_Options'Class;
      Count : Argument_Count)
      return Routine_Options'Class
   is
   begin
      return Routine_Options'
        (Routine_Options (This) with delta
             Automatic_Arg_Count => False, Arg_Count => Count);
   end Set_Argument_Count;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (This : in out Instance;
      L    : Label)
   is
   begin
      This.Routine_List (This.Active_Routine).Set_Labels.Append (L);
      This.Place_Label (L);
   end Set_Label;

   --------------------
   -- Set_No_Linkage --
   --------------------

   function Set_No_Linkage
     (This  : Routine_Options'Class)
      return Routine_Options'Class
   is
   begin
      return Routine_Options'
        (Routine_Options (This) with delta Linkage => False);
   end Set_No_Linkage;

   ------------------------
   -- Set_Trace_Callback --
   ------------------------

   procedure Set_Trace_Callback
     (Trace : Trace_Callback)
   is
   begin
      Trace_Proc := Trace;
   end Set_Trace_Callback;

   ---------------------
   -- Source_Location --
   ---------------------

   procedure Source_Location
     (This    : in out Instance;
      Line    : Positive;
      Column  : Positive)
   is
   begin
      This.Line := Line;
      This.Column := Column;
   end Source_Location;

   ---------------------
   -- String_Constant --
   ---------------------

   procedure String_Constant
     (This  : in out Instance;
      Value : String)
   is
      Length_List : Data_Lists.List;
      Char_List   : Data_Lists.List;
   begin
      Length_List.Append (Word_64_Element (Value'Length));
      Append_Data_List (This.RO_Data_List, 32, Length_List);

      for Ch of Value loop
         Char_List.Append (Word_64_Element (Character'Pos (Ch)));
      end loop;
      for I in 1 .. 4 - Value'Length mod 4 loop
         Char_List.Append (Word_64_Element (0));
      end loop;
      Append_Data_List (This.RO_Data_List, 32, Char_List);
   end String_Constant;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (This : in out Instance)
   is
   begin
      This.Append
        (Instruction_Record'(Stack, [], This.Line, This.Column, Swap));
   end Swap;

   -----------------
   -- To_Transfer --
   -----------------

   function To_Transfer
     (Src_1, Src_2 : Operand_Record;
      Op           : Operator;
      T            : Operand_Record)
      return Instruction_Record
   is
   begin
      if Src_1.Class = No_Operand
        and then Src_2.Class = Constant_Operand
        and then Op in Unary_Operator
      then
         return Instruction_Record'
           (Class  => Transfer,
            Labels => [],
            Line   => 1,
            Column => 1,
            Src_1  => No_Operand,
            Src_2  => Evaluate (Src_2, Op),
            T_Op   => Op_Identity,
            Dst    => T);
      elsif Src_1.Class = Constant_Operand
        and then Src_2.Class = Constant_Operand
        and then Op in Binary_Operator
        and then Op not in Op_Dereference | Op_Store
      then
         return Instruction_Record'
           (Class  => Transfer,
            Labels => [],
            Line   => 1,
            Column => 1,
            Src_1  => No_Operand,
            Src_2  => Evaluate (Src_1, Src_2, Op),
            T_Op   => Op_Identity,
            Dst    => T);
      elsif Op = Op_Dereference
        and then Src_1.Class = Stack_Operand
        and then Src_1.Reference
        and then Src_2.Word = 0
      then
         return Instruction_Record'
           (Class  => Transfer,
            Labels => [],
            Line   => 1,
            Column => 1,
            Src_1  => No_Operand,
            Src_2  => (Src_1 with delta Reference => False),
            T_Op   => Op_Identity,
            Dst    => T);
      else
         return Instruction_Record'
           (Class => Transfer,
            Labels => [],
            Line   => 1,
            Column => 1,
            Src_1  => Src_1,
            Src_2  => Src_2,
            T_Op   => Op,
            Dst    => T);
      end if;
   end To_Transfer;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Category : Trace_Category;
      Message  : String)
   is
   begin
      if Trace_Enabled (Category) and then Trace_Proc /= null then
         Trace_Proc (Message);
      end if;
   end Trace;

   ------------
   -- Update --
   ------------

   procedure Update
     (Routine : in out Routine_Record;
      From    : Instruction_Record)
   is
   begin
      case From.Class is
         when Push | Pop =>
            if From.Operand.Class = Stack_Operand then
               case From.Operand.Stack_Op is
                  when Argument_Operand =>
                     Routine.Last_Arg :=
                       Argument_Count'Max
                         (Routine.Last_Arg,
                          Argument_Index (From.Operand.Index));
                  when Local_Operand =>
                     declare
                        Loc : constant Local_Index :=
                                Local_Index (From.Operand.Index);
                     begin
                        Routine.Last_Loc :=
                          Local_Count'Max
                            (Routine.Last_Loc, Loc);
                        if Loc /= Routine.Last_Loc_Current then
                           Routine.Last_Loc_Current :=
                             Local_Count'Max
                               (Routine.Last_Loc_Current, Loc);
                        end if;
                     end;
                  when Result_Operand =>
                     Routine.Last_Res :=
                       Result_Count'Max
                         (Routine.Last_Res,
                          Result_Index (From.Operand.Index));
                  when Return_Operand =>
                     null;
               end case;
            end if;
         when others =>
            null;
      end case;
   end Update;

end Tagatha.Code;
