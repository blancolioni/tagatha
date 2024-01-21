package Tagatha.Names is

   type Tagatha_Name is private;

   function To_String (Name : Tagatha_Name) return String;
   function To_Name (Name : String) return Tagatha_Name;

private

   type Tagatha_Name is new Natural;

end Tagatha.Names;
