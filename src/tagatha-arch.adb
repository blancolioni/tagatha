with Ada.Text_IO;

with Tagatha.Arch.Loader;

package body Tagatha.Arch is

   ----------------
   -- Begin_Data --
   ----------------

   procedure Begin_Data
     (This       : in out Instance;
      Name       : String;
      Bits       : Natural;
      Read_Write : Boolean)
   is
   begin
      if Name /= "" then
         Dispatch (This).Name_Label (Name);
      end if;
      This.Data_Bits := Bits;
      This.Data_Buffer.Clear;
      This.RW_Data := Read_Write;
   end Begin_Data;

   -----------
   -- Datum --
   -----------

   procedure Datum
     (This  : in out Instance;
      Value : Word_64)
   is
   begin
      This.Label_Datum (Value'Image);
   end Datum;

   --------------
   -- End_Data --
   --------------

   procedure End_Data
     (This : in out Instance)
   is
   begin
      if not This.Data_Buffer.Is_Empty then
         Dispatch (This).Put_Data_Buffer;
      end if;
   end End_Data;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Instance'Class is
   begin
      return Loader.Get (Name);
   end Get;

   -----------------
   -- Label_Datum --
   -----------------

   procedure Label_Datum
     (This  : in out Instance;
      Label : String)
   is
   begin
      if This.Data_Buffer.Last_Index = 16 then
         Dispatch (This).Put_Data_Buffer;
      end if;
      This.Data_Buffer.Append (Label);
   end Label_Datum;

   -----------------
   -- Local_Label --
   -----------------

   procedure Local_Label
     (This  : in out Instance;
      Label : Positive)
   is
      Img : String := Label'Image;
   begin
      Img (Img'First) := 'L';
      This.Put_Line (Img & ":");
   end Local_Label;

   ----------
   -- Note --
   ----------

   procedure Note
     (This : in out Instance;
      Name : String;
      Tag  : Word_32;
      Text : String)
   is
   begin
      Dispatch (This).Put_Instruction
        ("note",
         Name,
         Tag'Image,
         '"' & Text & '"');
   end Note;

   -----------------
   -- Put_Comment --
   -----------------

   procedure Put_Comment
     (This    : in out Instance;
      Comment : String)
   is
   begin
      This.Lines.Append ("; " & Comment);
   end Put_Comment;

   ---------------------
   -- Put_Instruction --
   ---------------------

   procedure Put_Instruction
     (This        : in out Instance'Class;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
   is
      S : constant String :=
            "    "
            & Instruction
            & (if Arg_1 /= ""
               then " " & Arg_1
               & (if Arg_2 /= ""
                 then ", " & Arg_2
                 & (if Arg_3 /= ""
                   then ", " & Arg_3
                   else "")
                 else "")
               else "");
   begin
      This.Put_Line (S);
   end Put_Instruction;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (This : in out Instance'Class;
      Line : String)
   is
   begin
      if This.Location_Changed then
         This.Put_Comment (This.Line'Image & This.Column'Image);
         This.Location_Changed := False;
      end if;
      This.Lines.Append (Line);
   end Put_Line;

   ----------
   -- Save --
   ----------

   procedure Save
     (This : Instance'Class;
      Path : String)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for Line of This.Lines loop
         Put_Line (File, Line);
      end loop;
      Close (File);
   end Save;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option
     (This : in out Instance'Class;
      Item : Generate_Option)
   is
   begin
      This.Options (Item) := True;
   end Set_Option;

   -------------------------
   -- Set_Source_Location --
   -------------------------

   procedure Set_Source_Location
     (This   : in out Instance'Class;
      Line   : Positive;
      Column : Positive)
   is
   begin
      if This.Line /= Line or else This.Column /= Column then
         This.Line := Line;
         This.Column := Column;
         This.Location_Changed := True;
      end if;
   end Set_Source_Location;

end Tagatha.Arch;
