private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Text_Buffers;
private with Tagatha.Conversions;
private with Tagatha.Names;

with Tagatha.Arch;

package Tagatha.Code is

   type Label is private;
   No_Label : constant Label;

   function Has_Label (L : Label) return Boolean;

   type Instance is tagged private;

   function Next_Label
     (This : in out Instance)
      return Label;

   procedure Set_Label
     (This : in out Instance;
      L    : Label);

   procedure Source_Location
     (This    : in out Instance;
      Line    : Positive;
      Column  : Positive);

   procedure Push_Constant
     (This  : in out Instance;
      Value : Int_32);

   procedure Push_Constant
     (This    : in out Instance;
      Value   : Word_64;
      Content : Operand_Content := General_Content);

   procedure Push_Constant
     (This  : in out Instance;
      Value : Floating_Point_Constant);

   procedure Push_Argument
     (This    : in out Instance;
      Index   : Argument_Index;
      Content : Operand_Content := General_Content);

   procedure Pop_Argument
     (This  : in out Instance;
      Index   : Argument_Index;
      Content : Operand_Content := General_Content);

   procedure Push_Local
     (This      : in out Instance;
      Index     : Local_Index;
      Content   : Operand_Content := General_Content;
      Reference : Boolean := False);

   procedure Pop_Local
     (This  : in out Instance;
      Index   : Local_Index;
      Content : Operand_Content := General_Content);

   procedure Pop_Result
     (This  : in out Instance;
      Index   : Result_Index;
      Content : Operand_Content := General_Content);

   procedure Push_Return
     (This  : in out Instance;
      Index   : Return_Index;
      Content : Operand_Content := General_Content);

   procedure Push_Name
     (This     : in out Instance;
      Name     : String;
      Extern   : Boolean;
      Content  : Operand_Content := General_Content;
      Address  : Boolean := False);

   procedure Pop_Name
     (This    : in out Instance;
      Name    : String;
      Extern  : Boolean;
      Content : Operand_Content := General_Content);

   procedure Pop_Indirect
     (This    : in out Instance;
      Content : Operand_Content := General_Content;
      Offset  : Int_32 := 0);

   procedure Dereference
     (This    : in out Instance;
      Content : Operand_Content;
      Offset  : Int_32);

   procedure Dereference
     (This    : in out Instance;
      Offset  : Int_32 := 0);

   procedure Drop
     (This : in out Instance);

   procedure Pop
     (This : in out Instance);
   --  pop top two values from stack, store second in first

   procedure Duplicate
     (This : in out Instance);

   procedure Swap
     (This : in out Instance);

   procedure Operate
     (This : in out Instance;
      Op   : Operator);

   procedure Branch
     (This        : in out Instance;
      Condition   : Branch_Condition;
      Destination : Label);

   procedure Branch
     (This        : in out Instance;
      Destination : Label);

   procedure Call
     (This           : in out Instance;
      Name           : String;
      Argument_Count : Natural;
      Result_Count   : Natural);

   procedure Indirect_Call
     (This           : in out Instance;
      Argument_Count : Natural;
      Result_Count   : Natural);

   procedure Jump
     (This           : in out Instance;
      Name           : String);

   function Add_Local
     (This : in out Instance)
      return Local_Index;

   procedure Remove_Local
     (This : in out Instance);

   procedure Data_Label
     (This : in out Instance;
      Name : String);

   procedure String_Constant
     (This  : in out Instance;
      Value : String);

   procedure Data
     (This   : in out Instance;
      Values : Word_8_Array);

   procedure Data
     (This   : in out Instance;
      Value  : Int_32);

   procedure Data
     (This   : in out Instance;
      Label  : String);

   procedure Data_Label_RW
     (This : in out Instance;
      Name : String);

   procedure Data_RW
     (This   : in out Instance;
      Value  : Int_32);

   procedure Begin_Block
     (This : in out Instance);

   procedure End_Block
     (This : in out Instance);

   type Routine_Options is abstract tagged private;

   function Default_Options return Routine_Options'Class;

   function Set_Argument_Count
     (Count : Argument_Count)
      return Routine_Options'Class;

   function Set_Argument_Count
     (This  : Routine_Options'Class;
      Count : Argument_Count)
      return Routine_Options'Class;

   function Set_No_Linkage
      return Routine_Options'Class;

   function Set_No_Linkage
     (This  : Routine_Options'Class)
      return Routine_Options'Class;

   procedure Begin_Routine
     (This    : in out Instance;
      Name    : String;
      Options : Routine_Options'Class := Default_Options);

   procedure End_Routine
     (This : in out Instance);

   procedure Exception_Handler
     (This          : in out Instance;
      Start_Label   : String;
      End_Label     : String;
      Handler_Label : String);

   procedure Note
     (This : in out Instance;
      Name : String;
      Tag  : Word_32;
      Text : String);

   procedure Generate
     (This   : in out Instance;
      Target : in out Tagatha.Arch.Instance'Class);

   procedure Save
     (This : Instance;
      Path : String);

private

   type Label is new Natural;
   No_Label : constant Label := 0;

   function Has_Label (L : Label) return Boolean
   is (L /= No_Label);

   type Routine_Options is tagged
      record
         Automatic_Arg_Count : Boolean := True;
         Arg_Count           : Argument_Count := 0;
         Linkage             : Boolean := True;
      end record;

   function Set_Argument_Count
     (Count : Argument_Count)
      return Routine_Options'Class
   is (Default_Options.Set_Argument_Count (Count));

   function Set_No_Linkage
     return Routine_Options'Class
   is (Default_Options.Set_No_Linkage);

   type Instruction_Class is
     (Block, Branch, Call, Operate, Pop, Push, Stack, Transfer);

   type Stack_Instruction_Class is
     (Drop, Duplicate, Pop, Swap);

   type Operand_Class is
     (No_Operand,
      Stack_Operand,
      Constant_Operand,
      External_Operand,
      Temporary_Operand);

   type Stack_Operand_Type is
     (Argument_Operand,
      Local_Operand,
      Result_Operand,
      Return_Operand);

   type Operand_Index is range 1 .. 99;

   type Bit_Count is range 0 .. 64;

   type Operand_Record (Class : Operand_Class := No_Operand) is
      record
         Dereference : Boolean := False;
         Offset      : Int_32  := 0;
         Content     : Operand_Content := General_Content;
         case Class is
            when No_Operand =>
               null;
            when Stack_Operand =>
               Stack_Op  : Stack_Operand_Type;
               Index     : Operand_Index;
               Reference : Boolean := False;
            when Constant_Operand =>
               Bits     : Bit_Count;
               Word     : Word_64;
            when External_Operand =>
               Name     : Tagatha.Names.Tagatha_Name;
               Address  : Boolean;
               Imported : Boolean;
            when Temporary_Operand =>
               Temp     : Temporary_Index;
         end case;
      end record
     with Put_Image => Put_Operand_Image;

   procedure Put_Operand_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Operand_Record);

   function No_Operand return Operand_Record
   is (Class => No_Operand, others => <>);

   function Argument_Operand
     (Index   : Argument_Index;
      Content : Operand_Content)
      return Operand_Record
   is (Class    => Stack_Operand,
       Stack_Op => Argument_Operand,
       Index    => Operand_Index (Index),
       Content  => Content,
       others   => <>);

   function Constant_Operand
     (Value : Int_32)
      return Operand_Record
   is (Constant_Operand, False, 0, General_Content, 32,
       Tagatha.Conversions.Int_32_To_Word_64 (Value));

   function Constant_Operand
     (Value : Floating_Point_Constant)
      return Operand_Record
   is (Constant_Operand, False, 0, Floating_Point_Content, 32,
       Tagatha.Conversions.Floating_Point_To_Word_64 (Value));

   function Constant_Operand
     (Value   : Word_64;
      Content : Operand_Content)
      return Operand_Record
   is (Constant_Operand, False, 0, Content, 32, Value);

   function Name_Operand
     (Name     : String;
      Content  : Operand_Content;
      Address  : Boolean;
      Imported : Boolean)
      return Operand_Record
   is (External_Operand, False, 0, Content,
       Tagatha.Names.To_Name (Name), Address, Imported);

   function Local_Operand
     (Index     : Local_Index;
      Content   : Operand_Content;
      Reference : Boolean := False)
      return Operand_Record
   is (Class     => Stack_Operand,
       Stack_Op  => Local_Operand,
       Index     => Operand_Index (Index),
       Content   => Content,
       Reference => Reference,
       others   => <>);

   function Result_Operand
     (Index   : Result_Index;
      Content : Operand_Content)
      return Operand_Record
   is (Class    => Stack_Operand,
       Stack_Op => Result_Operand,
       Index    => Operand_Index (Index),
       Content  => Content,
       others   => <>);

   function Return_Operand
     (Index   : Return_Index;
      Content : Operand_Content)
      return Operand_Record
   is (Class    => Stack_Operand,
       Stack_Op => Return_Operand,
       Index    => Operand_Index (Index),
       Content  => Content,
       others   => <>);

   package Label_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Label);

   package Operand_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Operand_Record);

   type Instruction_Record (Class : Instruction_Class) is
      record
         Labels : Label_Lists.List;
         Line   : Positive;
         Column : Positive;
         case Class is
            when Block =>
               Begin_Block    : Boolean;
            when Branch =>
               Branch_Op      : Operand_Record;
               Condition      : Branch_Condition;
               Branch_To      : Label;
               Forward        : Boolean;
            when Call =>
               Name           : Operand_Record;
               Argument_Count : Natural;
               Result_Count   : Natural;
               Actuals        : Operand_Lists.List;
               Is_Subroutine  : Boolean;
               Is_Indirect    : Boolean;
            when Operate =>
               Op             : Operator;
            when Push | Pop =>
               Operand        : Operand_Record;
            when Stack =>
               Stack_Op       : Stack_Instruction_Class;
            when Transfer =>
               Src_1, Src_2   : Operand_Record;
               T_Op           : Operator;
               Dst            : Operand_Record;
         end case;
      end record
     with Put_Image => Put_Instruction_Image;

   procedure Put_Instruction_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Instruction_Record);

   package Instruction_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Instruction_Record);

   type Temporary_Record is
      record
         First_Write : Natural := 0;
         Last_Read   : Natural := 0;
         Content     : Operand_Content := General_Content;
         Remap       : Operand_Record;
      end record;

   package Temporary_Vectors is
     new Ada.Containers.Vectors (Temporary_Index, Temporary_Record);

   type Routine_Record is
      record
         Name             : Tagatha.Names.Tagatha_Name;
         Options          : Routine_Options;
         Stack_Code       : Instruction_Vectors.Vector;
         Transfer_Code    : Instruction_Vectors.Vector;
         Temporaries      : Temporary_Vectors.Vector;
         Set_Labels       : Label_Lists.List;
         Last_Loc_Current : Local_Count    := 0;
         Last_Arg         : Argument_Count := 0;
         Last_Loc         : Local_Count    := 0;
         Last_Res         : Result_Count   := 0;
      end record;

   package Routine_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Routine_Record);

   type Label_Record is
      record
         Referenced : Boolean := False;
         Placed     : Boolean := False;
      end record;

   subtype Real_Label is Label range 1 .. Label'Last;

   package Label_Vectors is
     new Ada.Containers.Vectors (Real_Label, Label_Record);

   type Data_Element_Class is
     (Word_64_Element, Label_Element);

   type Data_Element_Record (Class : Data_Element_Class := Word_64_Element) is
      record
         case Class is
            when Word_64_Element =>
               Value : Word_64;
            when Label_Element =>
               Name  : Tagatha.Names.Tagatha_Name;
         end case;
      end record;

   function Word_64_Element (Value : Word_64) return Data_Element_Record
   is (Word_64_Element, Value);

   function Label_Element (Name : String) return Data_Element_Record
   is (Label_Element, Tagatha.Names.To_Name (Name));

   package Data_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Data_Element_Record);

   type Labeled_Data is
      record
         Name : Tagatha.Names.Tagatha_Name;
         Bits : Bit_Count;
         Data : Data_Lists.List;
      end record;

   package Labeled_Data_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Labeled_Data);

   type Note_Record is
      record
         Name        : Tagatha.Names.Tagatha_Name;
         Tag         : Word_32;
         Description : Tagatha.Names.Tagatha_Name;
      end record;

   package Note_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Note_Record);

   type Instance is tagged
      record
         Routine_List   : Routine_Lists.List;
         RO_Data_List   : Labeled_Data_Lists.List;
         RW_Data_List   : Labeled_Data_Lists.List;
         Notes          : Note_Lists.List;
         Active_Routine : Routine_Lists.Cursor;
         Label_Vector   : Label_Vectors.Vector;
         Last_Label     : Label := No_Label;
         Last_Temp      : Temporary_Count := 0;
         Line           : Positive := 1;
         Column         : Positive := 1;
      end record;

   procedure Branch_To_Label
     (This    : in out Instance'Class;
      L       : Label;
      Forward : out Boolean);

   procedure Place_Label
     (This : in out Instance'Class;
      L    : Label);

   function Next_Temporary
     (This : in out Instance'Class)
      return Temporary_Index;

   procedure Append
     (This : in out Instance'Class;
      Item : Instruction_Record)
     with Pre => Routine_Lists.Has_Element (This.Active_Routine);

   procedure Pop
     (This : in out Instance'Class;
      Item : Operand_Record);

   procedure Push
     (This : in out Instance'Class;
      Item : Operand_Record);

   procedure Append_Data_List
     (Data_List : in out Labeled_Data_Lists.List;
      Bits      : Bit_Count;
      List      : Data_Lists.List);

   type Code_Change_Operation is
     (Replace_Instruction,
      Remap_Temporary);

   package Instruction_Holders is
     new Ada.Containers.Indefinite_Holders (Instruction_Record);

   type Code_Change (Operation : Code_Change_Operation) is
      record
         case Operation is
            when Replace_Instruction =>
               First, Last     : Natural;
               New_Instruction : Instruction_Holders.Holder;
            when Remap_Temporary =>
               Temp            : Temporary_Index;
               Map_To          : Operand_Record;
         end case;
      end record;

   function Replace (First, Last : Natural;
                     New_Instr : Instruction_Record)
                     return Code_Change
   is (Replace_Instruction, First, Last,
       Instruction_Holders.To_Holder (New_Instr));

   function Remap (Temporary : Temporary_Index;
                   Map_To    : Operand_Record)
                   return Code_Change
   is (Remap_Temporary, Temporary, Map_To);

   package Code_Change_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Code_Change);

end Tagatha.Code;
