with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;

package body Tagatha.Names is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Tagatha_Name, String);

   package String_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Tagatha_Name,
        Ada.Strings.Hash_Case_Insensitive,
        Ada.Strings.Equal_Case_Insensitive);

   Name_Vector : String_Vectors.Vector := [""];
   Name_Map    : String_Maps.Map := [["", 0]];

   -------------
   -- To_Name --
   -------------

   function To_Name (Name : String) return Tagatha_Name is
      use String_Maps;
      Position : constant Cursor := Name_Map.Find (Name);
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         Name_Vector.Append (Name);
         Name_Map.Insert (Name, Name_Vector.Last_Index);
         return Name_Vector.Last_Index;
      end if;
   end To_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (Name : Tagatha_Name) return String is
   begin
      return Name_Vector (Name);
   end To_String;

end Tagatha.Names;
