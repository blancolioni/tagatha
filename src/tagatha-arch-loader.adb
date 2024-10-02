with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;

with Tagatha.Arch.Aqua;
with Tagatha.Arch.M6502;
with Tagatha.Arch.Pdp11;

package body Tagatha.Arch.Loader is

   function Load_6502 return Instance'Class;
   function Load_Aqua return Instance'Class;
   function Load_Pdp11 return Instance'Class;

   package Arch_Loader_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Arch_Loader,
        Ada.Strings.Hash_Case_Insensitive,
        Ada.Strings.Equal_Case_Insensitive);

   Arch_Loader_Map : Arch_Loader_Maps.Map :=
     ["aqua" => Load_Aqua'Access,
      "6502" => Load_6502'Access,
      "pdp11" => Load_Pdp11'Access
     ];

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Instance'Class is
   begin
      if Arch_Loader_Map.Contains (Name) then
         return Arch_Loader_Map.Element (Name).all;
      else
         raise Constraint_Error with
           "unknown architecture: " & Name;
      end if;
   end Get;

   ---------------
   -- Load_6502 --
   ---------------

   function Load_6502 return Instance'Class is
   begin
      return Target : Tagatha.Arch.M6502.Instance;
   end Load_6502;

   ---------------
   -- Load_Aqua --
   ---------------

   function Load_Aqua return Instance'Class is
   begin
      return Target : Tagatha.Arch.Aqua.Instance;
   end Load_Aqua;

   ----------------
   -- Load_Pdp11 --
   ----------------

   function Load_Pdp11 return Instance'Class is
   begin
      return Target : Tagatha.Arch.Pdp11.Instance;
   end Load_Pdp11;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name : String;
      Loader : Arch_Loader)
   is
   begin
      Arch_Loader_Map.Insert (Name, Loader);
   end Register;

end Tagatha.Arch.Loader;
