private with Ada.Containers.Indefinite_Vectors;

package Tagatha.Arch.Aqua is

   subtype Parent is Arch.Instance;
   type Instance is new Parent with private;

private

   Last_Register : constant := 255;
   Last_Local    : constant := 239;
   type Register_Index is range 0 .. Last_Register;
   subtype Local_Register_Index is Register_Index range 0 .. Last_Local;

   function Show (Index : Register_Index) return String;

   type Register_State is
      record
         Claimed    : Boolean := False;
         Assignment : Temporary_Count := 0;
      end record;

   type Register_State_Array is
     array (Register_Index) of Register_State;

   type Aqua_Operand_Instance is abstract new Operand_Interface with
      record
         R        : Register_Index;
         Content  : Operand_Content;
      end record;

   function Is_Register_Operand
     (This : Aqua_Operand_Instance)
      return Boolean
   is (True);

   package Indirect_Label_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Instance is new Parent with
      record
         First_Arg        : Register_Index;
         Arg_Bound        : Register_Index;
         First_Result     : Register_Index;
         Result_Bound     : Register_Index;
         First_Local      : Register_Index;
         Local_Bound      : Register_Index;
         First_Temp       : Register_Index;
         Temp_Bound       : Register_Index;
         Call_Return      : Register_Index;
         Temps            : Register_State_Array;
         Data_Last        : Natural := 0;
         Saved_J          : Register_Index;
         Linkage          : Boolean;
         Next_Indirect    : Natural := 0;
         Last_Ind_Written : Natural := 0;
         Indirect_Vector  : Indirect_Label_Vectors.Vector;
      end record;

   procedure Move_To_Register
     (Operand     : Aqua_Operand_Instance;
      This        : in out Instance'Class;
      Destination : Register_Index);

   procedure Set_From_Register
     (Operand : Aqua_Operand_Instance;
      This    : in out Instance'Class;
      Source  : Register_Index);

   overriding procedure Name_Label
     (This : in out Instance;
      Name : String);

   overriding procedure Local_Label
     (This  : in out Instance;
      Label : Positive);

   overriding function No_Operand
     (This  : Instance)
      return Operand_Interface'Class;

   overriding function Argument_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Argument_Index)
      return Operand_Interface'Class;

   overriding function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Local_Index)
      return Operand_Interface'Class;

   overriding function Result_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Result_Index)
      return Operand_Interface'Class;

   overriding function Return_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index : Return_Index)
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
     (This : in out Instance;
      Name      : String;
      Arguments : Argument_Count;
      Results   : Result_Count;
      Locals    : Local_Count;
      Linkage   : Boolean);

   overriding procedure End_Routine
     (This : in out Instance);

   overriding procedure Exit_Routine
     (This        : in out Instance);

   overriding procedure Fail_Routine
     (This        : in out Instance);

   overriding procedure Retry
     (This        : in out Instance;
      Destination : String);

   overriding procedure Raise_Exception
     (This    : in out Instance;
      E       : Operand_Interface'Class);

   overriding procedure Transfer
     (This         : in out Instance;
      Dst          : Operand_Interface'Class;
      Src_1, Src_2 : Operand_Interface'Class;
      Op           : Operator);

   overriding procedure Begin_Data
     (This       : in out Instance;
      Name       : String;
      Bits       : Natural;
      Read_Write : Boolean);

   overriding procedure End_Data
     (This : in out Instance);

   overriding procedure Put_Data_Buffer (This : in out Instance);

   function Claim (This : in out Instance'Class) return Register_Index;
   procedure Release
     (This : in out Instance'Class;
      R    : Register_Index)
     with Pre => R in This.First_Temp .. This.Temp_Bound - 1
       and then This.Temps (R).Claimed;

   function Indirect_Label
     (This           : in out Instance;
      External_Label : String)
      return String;

end Tagatha.Arch.Aqua;
