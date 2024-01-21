with Ada.Text_IO;

package body Tagatha.Arch is

   ----------------
   -- Begin_Data --
   ----------------

   procedure Begin_Data
     (This : in out Instance;
      Name : String;
      Bits : Natural)
   is
   begin
      if Name /= "" then
         Dispatch (This).Name_Label (Name);
      end if;
      This.Data_Bits := Bits;
      This.Data_Buffer.Clear;
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
         This.Lines.Append (";" & This.Line'Image & This.Column'Image);
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
