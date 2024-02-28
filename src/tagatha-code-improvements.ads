private package Tagatha.Code.Improvements is

   type Improver_Interface is interface;

   function Name
     (This : Improver_Interface)
      return String
      is abstract;

   function Test
     (This          : Improver_Interface;
      Current, Next : Instruction_Record)
      return Boolean
      is abstract;

   function Fix
     (This          : Improver_Interface;
      Current, Next : Instruction_Record)
      return Instruction_Record
   is abstract;

   procedure Iterate_Improvements
     (Process : not null access
        procedure (Improvement : Improver_Interface'Class;
                   Stop        : out Boolean));

end Tagatha.Code.Improvements;
