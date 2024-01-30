private with Tagatha.Names;

package Tagatha.Arch.M6502 is

   subtype Parent is Arch.Instance;
   type Instance is new Parent with private;

private

   --  6502 code
   --  Registers are really zero-page addresses.  We take eight of them.
   --
   type Register_Index is range 0 .. 7;
   subtype Temporary_Register_Index is
     Register_Index range 1 .. 7;

   type Register_Assignments is
     array (Temporary_Register_Index) of Temporary_Count;

   type Accumulator_Index is range 0 .. 3;
   subtype Temporary_Accumulator_Index is
     Accumulator_Index range 2 .. 3;

   type Accumulator_Assignments is
     array (Temporary_Accumulator_Index) of Temporary_Count;

   type Instance is new Parent with
      record
         Temps     : Register_Assignments    := [others => 0];
         Acc_Temps : Accumulator_Assignments := [others => 0];
         Routine   : Tagatha.Names.Tagatha_Name;
         A, X, Y   : Tagatha.Names.Tagatha_Name := Tagatha.Names.Empty_Name;
         Next_ZP   : Word_64 := 16#A0#;
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

   overriding function External_Operand
     (This    : Instance;
      Name    : String;
      Address : Boolean)
      return Operand_Interface'Class;

   overriding function Temporary_Operand
     (This        : in out Instance;
      Index       : Temporary_Index;
      Content     : Operand_Content;
      First_Write : Boolean;
      Last_Read   : Boolean)
      return Operand_Interface'Class;

   overriding procedure Local_Label
     (This  : in out Instance;
      Label : Positive);

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
      Locals    : Local_Count);

   overriding procedure End_Routine
     (This : in out Instance);

   overriding procedure Transfer
     (This         : in out Instance;
      Dst          : Operand_Interface'Class;
      Src_1, Src_2 : Operand_Interface'Class;
      Op           : Operator);

   overriding procedure Put_Data_Buffer (This : in out Instance);

   overriding procedure Put_Comment
     (This    : in out Instance;
      Comment : String);

   type M6502_Operand_Instance is
     abstract new Operand_Interface with
      record
         Content : Operand_Content;
      end record;

   function Word_Size
     (This : M6502_Operand_Instance)
      return Positive
   is (1);

   function Is_Accumulator (This : M6502_Operand_Instance) return Boolean
   is (False);

   function Is_Constant
     (This  : M6502_Operand_Instance;
      Value : Integer)
      return Boolean
   is (False);

   function Is_Zero (This : M6502_Operand_Instance'Class) return Boolean
   is (This.Is_Constant (0));

   function Offset_Image
     (This        : M6502_Operand_Instance;
      Src         : Boolean;
      Word_Offset : Natural  := 0)
      return String
   is (M6502_Operand_Instance'Class (This).Image);

   procedure LDA
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class);

   procedure LDX
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class);

   procedure STA
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class);

   procedure ADC
     (This    : in out Instance'Class;
      Operand : Operand_Interface'Class);

end Tagatha.Arch.M6502;
