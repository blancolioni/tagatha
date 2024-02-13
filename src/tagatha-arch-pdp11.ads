package Tagatha.Arch.Pdp11 is

   subtype Parent is Arch.Instance;
   type Instance is new Parent with private;

private

   type Register_Index is range 0 .. 7;
   subtype Temporary_Register_Index is
     Register_Index range 2 .. 4;

   type Register_Assignments is
     array (Temporary_Register_Index) of Temporary_Count;

   type Accumulator_Index is range 0 .. 5;
   subtype Temporary_Accumulator_Index is
     Accumulator_Index range 2 .. 5;

   type Accumulator_Assignments is
     array (Temporary_Accumulator_Index) of Temporary_Count;

   type Instance is new Parent with
      record
         Temps     : Register_Assignments    := [others => 0];
         Acc_Temps : Accumulator_Assignments := [others => 0];
      end record;

   overriding function No_Operand
     (This  : Instance)
      return Operand_Interface'Class;

   overriding function Argument_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Argument_Index)
      return Operand_Interface'Class;

   overriding function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Local_Index)
      return Operand_Interface'Class;

   overriding function Result_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Result_Index)
      return Operand_Interface'Class;

   overriding function Return_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Return_Index)
      return Operand_Interface'Class;

   overriding function Constant_Operand
     (This     : Instance;
      Content  : Operand_Content;
      Value    : Word_64)
      return Operand_Interface'Class;

   overriding function Name_Operand
     (This    : Instance;
      Name    : String;
      Address  : Boolean;
      Imported : Boolean)
      return Operand_Interface'Class;

   overriding function Temporary_Operand
     (This        : in out Instance;
      Index       : Temporary_Index;
      Content     : Operand_Content;
      First_Write : Boolean;
      Last_Read   : Boolean)
      return Operand_Interface'Class;

   overriding procedure Name_Label
     (This : in out Instance;
      Name : String);

   overriding procedure Branch
     (This        : in out Instance;
      Operand     : Operand_Interface'Class;
      Condition   : Branch_Condition;
      Destination : Positive;
      Forward     : Boolean);

   overriding procedure Call
     (This           : in out Instance;
      Name           : Operand_Interface'Class;
      Actuals        : Operand_Lists.List;
      Result_Count   : Natural);

   overriding procedure Jump
     (This           : in out Instance;
      Name           : Operand_Interface'Class);

   overriding procedure Begin_Routine
     (This      : in out Instance;
      Name      : String;
      Arguments : Argument_Count;
      Results   : Result_Count;
      Locals    : Local_Count;
      Linkage   : Boolean);

   overriding procedure End_Routine
     (This : in out Instance);

   overriding procedure Transfer
     (This         : in out Instance;
      Dst          : Operand_Interface'Class;
      Src_1, Src_2 : Operand_Interface'Class;
      Op           : Operator);

   overriding procedure Put_Data_Buffer (This : in out Instance);

   type Pdp11_Operand_Instance is
     abstract new Operand_Interface with
      record
         Content : Operand_Content;
      end record;

   function Word_Size
     (This : Pdp11_Operand_Instance)
      return Positive
   is (1);

   function Offset_Image
     (This        : Pdp11_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String
   is (Pdp11_Operand_Instance'Class (This).Image);

end Tagatha.Arch.Pdp11;
