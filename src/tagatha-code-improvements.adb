with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Tagatha.Code.Improvements is

   type Redundant_Branch_To_Next is
     new Improver_Interface with null record;

   overriding function Name
     (This : Redundant_Branch_To_Next)
      return String
   is ("remove redundant branch to next instruction");

   overriding function Test
     (This          : Redundant_Branch_To_Next;
      Current, Next : Instruction_Record)
      return Boolean;

   overriding function Fix
     (This          : Redundant_Branch_To_Next;
      Current, Next : Instruction_Record)
      return Code_Change_Lists.List;

   type Not_Followed_By_Conditional_Branch is
     new Improver_Interface with null record;

   overriding function Name
     (This : Not_Followed_By_Conditional_Branch)
      return String
   is ("replace condition in branch preceded by not "
       & "with branch on the negation of the condition");

   overriding function Test
     (This          : Not_Followed_By_Conditional_Branch;
      Current, Next : Instruction_Record)
      return Boolean;

   overriding function Fix
     (This          : Not_Followed_By_Conditional_Branch;
      Current, Next : Instruction_Record)
      return Code_Change_Lists.List;

   type Operation_And_Self_Assignment is
     new Improver_Interface with null record;

   overriding function Name
     (This : Operation_And_Self_Assignment)
      return String
   is ("replace transfer to temporary then to non-temporary "
       & "with a direct transfer to temporary");

   overriding function Test
     (This          : Operation_And_Self_Assignment;
      Current, Next : Instruction_Record)
      return Boolean;

   overriding function Fix
     (This          : Operation_And_Self_Assignment;
      Current, Next : Instruction_Record)
      return Code_Change_Lists.List;

   package Improvement_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Improver_Interface'Class);

   Improvement_List : constant Improvement_Lists.List :=
                        [Redundant_Branch_To_Next'(null record),
                         Not_Followed_By_Conditional_Branch'(null record),
                         Operation_And_Self_Assignment'(null record)
                        ];

   ---------
   -- Fix --
   ---------

   overriding function Fix
     (This          : Redundant_Branch_To_Next;
      Current, Next : Instruction_Record)
      return Code_Change_Lists.List
   is
   begin
      return [Replace (0, 1, Next)];
   end Fix;

   ---------
   -- Fix --
   ---------

   overriding function Fix
     (This          : Not_Followed_By_Conditional_Branch;
      Current, Next : Instruction_Record)
      return Code_Change_Lists.List
   is
      Instr : Instruction_Record := Next;
   begin
      Instr.Condition :=
        (if Next.Condition = Z then NZ else Z);
      Instr.Branch_Op := Current.Src_2;
      return [Replace (0, 1, Instr)];
   end Fix;

   ---------
   -- Fix --
   ---------

   overriding function Fix
     (This          : Operation_And_Self_Assignment;
      Current, Next : Instruction_Record)
      return Code_Change_Lists.List
   is
      New_Instr : Instruction_Record := Current;
   begin
      New_Instr.Dst := Next.Dst;
      return [Replace (0, 1, New_Instr),
              Remap (Current.Dst.Temp, New_Instr.Dst)];
   end Fix;

   --------------------------
   -- Iterate_Improvements --
   --------------------------

   procedure Iterate_Improvements
     (Process : not null access
        procedure (Improvement : Improver_Interface'Class;
                   Stop        : out Boolean))
   is
   begin
      for Improvement of Improvement_List loop
         declare
            Stop : Boolean;
         begin
            Process (Improvement, Stop);
            exit when Stop;
         end;
      end loop;
   end Iterate_Improvements;

   ----------
   -- Test --
   ----------

   overriding function Test
     (This          : Redundant_Branch_To_Next;
      Current, Next : Instruction_Record)
      return Boolean
   is
   begin
      return Current.Class = Branch
        and then Next.Labels.Contains (Current.Branch_To)
        and then Current.Forward;
   end Test;

   ----------
   -- Test --
   ----------

   overriding function Test
     (This          : Not_Followed_By_Conditional_Branch;
      Current, Next : Instruction_Record)
      return Boolean
   is
   begin
      return Next.Class = Branch
        and then Next.Condition /= Always
        and then Current.Class = Transfer
        and then Current.T_Op = Op_Not
        and then Current.Dst = Next.Branch_Op
        and then Current.Dst.Class = Temporary_Operand;
   end Test;

   ----------
   -- Test --
   ----------

   overriding function Test
     (This          : Operation_And_Self_Assignment;
      Current, Next : Instruction_Record)
      return Boolean
   is
   begin
      return Current.Class = Transfer
        and then Current.Dst.Class = Temporary_Operand
        and then Next.Class = Transfer
        and then Next.T_Op = Op_Identity
        and then Current.Dst = Next.Src_2;
   end Test;

end Tagatha.Code.Improvements;
