private package Tagatha.Arch.Loader is

   type Arch_Loader is access
     function return Instance'Class;

   procedure Register
     (Name : String;
      Loader : Arch_Loader);

   function Get (Name : String) return Instance'Class;

end Tagatha.Arch.Loader;
