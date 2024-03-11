private with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Tagatha.Arch is

   type Operand_Interface is interface;

   function Image (This : Operand_Interface) return String is abstract;

   type Generate_Option is (No_Recursion);

   type Instance is abstract tagged private;

   procedure Set_Option
     (This : in out Instance'Class;
      Item : Generate_Option);

   function Option
     (This : Instance'Class;
      Item : Generate_Option)
      return Boolean;

   procedure Put_Line
     (This : in out Instance'Class;
      Line : String);

   procedure Put_Instruction
     (This        : in out Instance'Class;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "");

   procedure Put_Comment
     (This : in out Instance;
      Comment : String);

   procedure Set_Source_Location
     (This   : in out Instance'Class;
      Line   : Positive;
      Column : Positive);

   procedure Name_Label
     (This : in out Instance;
      Name : String)
   is abstract;

   procedure Local_Label
     (This  : in out Instance;
      Label : Positive);

   function No_Operand
     (This  : Instance)
      return Operand_Interface'Class
      is abstract;

   function Argument_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Argument_Index)
      return Operand_Interface'Class
      is abstract;

   function Local_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Local_Index)
      return Operand_Interface'Class
      is abstract;

   function Result_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Result_Index)
      return Operand_Interface'Class
      is abstract;

   function Return_Operand
     (This    : Instance;
      Content : Operand_Content;
      Index   : Return_Index)
      return Operand_Interface'Class
      is abstract;

   function Constant_Operand
     (This     : Instance;
      Content  : Operand_Content;
      Value    : Word_64)
      return Operand_Interface'Class
      is abstract;

   function Name_Operand
     (This    : Instance;
      Name    : String;
      Address : Boolean;
      Imported : Boolean)
      return Operand_Interface'Class
      is abstract;

   function Temporary_Operand
     (This        : in out Instance;
      Index       : Temporary_Index;
      Content     : Operand_Content;
      First_Write : Boolean;
      Last_Read   : Boolean)
      return Operand_Interface'Class
      is abstract;

   procedure Branch
     (This        : in out Instance;
      Operand     : Operand_Interface'Class;
      Condition   : Branch_Condition;
      Destination : Positive;
      Forward     : Boolean)
   is abstract;

   package Operand_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Operand_Interface'Class);

   procedure Call
     (This         : in out Instance;
      Destination  : Operand_Interface'Class;
      Actuals      : Operand_Lists.List;
      Result_Count : Natural)
   is abstract;

   procedure Jump
     (This         : in out Instance;
      Destination  : Operand_Interface'Class)
   is abstract;

   procedure Begin_Routine
     (This      : in out Instance;
      Name      : String;
      Arguments : Argument_Count;
      Results   : Result_Count;
      Locals    : Local_Count;
      Linkage   : Boolean)
   is abstract;

   procedure End_Routine
     (This : in out Instance)
   is abstract;

   procedure Begin_Data
     (This       : in out Instance;
      Name       : String;
      Bits       : Natural;
      Read_Write : Boolean);

   procedure Datum
     (This  : in out Instance;
      Value : Word_64);

   procedure Label_Datum
     (This  : in out Instance;
      Label : String);

   procedure End_Data
     (This : in out Instance);

   procedure Put_Data_Buffer
     (This : in out Instance)
   is abstract;

   procedure Transfer
     (This         : in out Instance;
      Dst          : Operand_Interface'Class;
      Src_1, Src_2 : Operand_Interface'Class;
      Op           : Operator)
   is abstract;

   procedure Note
     (This : in out Instance;
      Name : String;
      Tag  : Word_32;
      Text : String);

   procedure Save
     (This : Instance'Class;
      Path : String);

   function Get (Name : String) return Instance'Class;

private

   package Line_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Data_Buffer_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Generate_Option_Array is array (Generate_Option) of Boolean;

   type Instance is abstract tagged
      record
         Location_Changed : Boolean := True;
         Line             : Positive := 1;
         Column           : Positive := 1;
         Lines            : Line_Lists.List;
         Data_Bits        : Natural := 0;
         Data_Buffer      : Data_Buffer_Vectors.Vector;
         RW_Data          : Boolean := False;
         Options          : Generate_Option_Array := [others => False];
      end record;

   subtype Dispatch is Instance'Class;

   function Option
     (This : Instance'Class;
      Item : Generate_Option)
      return Boolean
   is (This.Options (Item));

end Tagatha.Arch;
