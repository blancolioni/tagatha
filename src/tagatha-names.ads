package Tagatha.Names is

   type Tagatha_Name is private;

   Empty_Name : constant Tagatha_Name;

   function To_String (Name : Tagatha_Name) return String;
   function To_Name (Name : String) return Tagatha_Name;

private

   type Tagatha_Name is new Natural;

   Empty_Name : constant Tagatha_Name := 0;

end Tagatha.Names;
