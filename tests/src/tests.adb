with Ada.Command_Line;
with Ada.Text_IO;

with Tagatha.Arch.Aqua;
with Tagatha.Arch.M6502;
with Tagatha.Arch.Pdp11;
with Tagatha.Code;

procedure Tests is

   Failures : Natural := 0;

   procedure Next_3X_1;
   procedure Float_Frame;
   procedure Unused_Float_Argument;

   procedure Check
     (Name     : String;
      Path     : String;
      Expected : String);
   --  Fail unless Path contains a line whose trimmed text is Expected.

   -----------
   -- Check --
   -----------

   procedure Check
     (Name     : String;
      Path     : String;
      Expected : String)
   is
      use Ada.Text_IO;

      function Trim (S : String) return String;

      ----------
      -- Trim --
      ----------

      function Trim (S : String) return String is
         First : Positive := S'First;
         Last  : Natural  := S'Last;
      begin
         while First <= Last and then S (First) = ' ' loop
            First := First + 1;
         end loop;
         while Last >= First and then S (Last) = ' ' loop
            Last := Last - 1;
         end loop;
         return S (First .. Last);
      end Trim;

      File  : File_Type;
      Found : Boolean := False;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         if Trim (Get_Line (File)) = Expected then
            Found := True;
         end if;
      end loop;
      Close (File);

      if Found then
         Put_Line ("pass: " & Name);
      else
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name & ": no line """ & Expected
                   & """ in " & Path);
      end if;
   end Check;

   -----------------
   -- Float_Frame --
   -----------------

   procedure Float_Frame is
      --  double scale (double x, int n, double y) is
      --     local d : double;
      --  begin
      --     d := x * y;
      --     return d + x;
      --  end
      --
      --  Exercises Phase B: a double argument, then an integer one, then
      --  another double, plus a double local and a double result.  The
      --  integer sits between the doubles deliberately -- with the old
      --  one-register-per-slot layout its register would be wrong.
      --
      --  Expected Aqua frame: x at %0/%1, n at %2, y at %3/%4, the result at
      --  %5/%6, the local at %7/%8, and rJ saved in %9.
      Code : Tagatha.Code.Instance;
      Opts : constant Tagatha.Code.Routine_Options'Class :=
               Tagatha.Code.Set_Argument_Content
                 (1, Tagatha.Floating_Point_Content)
               .Set_Argument_Content (3, Tagatha.Floating_Point_Content)
               .Set_Result_Content (1, Tagatha.Floating_Point_Content);
   begin
      Code.Begin_Routine ("scale", Opts);

      --  d := x * y
      Code.Push_Argument (1, Tagatha.Floating_Point_Content);
      Code.Push_Argument (3, Tagatha.Floating_Point_Content);
      Code.Operate (Tagatha.Op_Fmul);
      Code.Pop_Local (1, Tagatha.Floating_Point_Content);

      --  result := d + x
      Code.Push_Local (1, Tagatha.Floating_Point_Content);
      Code.Push_Argument (1, Tagatha.Floating_Point_Content);
      Code.Operate (Tagatha.Op_Fadd);
      Code.Pop_Result (1, Tagatha.Floating_Point_Content);

      Code.Exit_Routine;
      Code.End_Routine;

      --  A caller: scale (1.5, 7, 2.0), keeping the double return.
      --  Actuals are pushed LAST first: Call pops them off the operand stack,
      --  so pushing in reverse is what leaves them in argument order (this is
      --  the convention Ack.Generate follows -- "for Item of reverse ...").
      --
      --  call_scale has no arguments and one double result, so its result
      --  pair is %0/%1, rJ is saved in %2, and the outgoing actuals start at
      --  %4: 1.5 in %4/%5, 7 in %6, 2.0 in %7/%8.  pushj %3 then puts the
      --  first actual at the callee's %0, and the double return comes back
      --  in %3/%4.
      Code.Begin_Routine ("call_scale");
      Code.Push_Constant (Tagatha.Floating_Point_Constant'(2.0));
      Code.Push_Constant (Tagatha.Int_32'(7));
      Code.Push_Constant (Tagatha.Floating_Point_Constant'(1.5));
      Code.Call ("scale", 3, 1,
                 Returns => [1      => Tagatha.Floating_Point_Content,
                             others => Tagatha.General_Content]);
      Code.Push_Return (1, Tagatha.Floating_Point_Content);
      Code.Pop_Result (1, Tagatha.Floating_Point_Content);
      Code.Exit_Routine;
      Code.End_Routine;

      Code.Save ("float_frame.lst");

      declare
         Target : Tagatha.Arch.Aqua.Instance;
      begin
         Code.Generate (Target);
         Target.Save ("float_frame.s");
      end;

      --  Callee frame: both source pairs and the destination pair.
      Check ("scale: d := x * y", "float_frame.s", "fmul %7, %0, %3");
      Check ("scale: result := d + x", "float_frame.s", "fadd %5, %7, %0");

      --  Result region is two registers wide, and pop counts registers.
      Check ("scale: pops two registers", "float_frame.s", "pop  2, 0");

      --  `pop n` rotates the callee's %0 .. %(n-1) left by one on the way to
      --  the caller, so Exit_Routine pre-rotates: word 1 (%6) to %0 and word 0
      --  (%5, stashed in the scratch %10) to %1.  Without this the caller gets
      --  the low word first -- see float_frame_direct.s.
      Check ("scale: exit rotates word 1 down to %0", "float_frame.s",
             "set %0, %6");
      Check ("scale: exit rotates word 0 up to %1", "float_frame.s",
             "set %1, %10");

      --  Caller: 1.5 high word to %4 (0x3FF8 = 16376), the integer to %6,
      --  2.0 high word to %7 (0x4000 = 16384).
      Check ("call: first actual is a pair at %4", "float_frame.s",
             "seth %4,  16376");
      Check ("call: integer actual displaced to %6", "float_frame.s",
             "setl %6,  7");
      Check ("call: third actual is a pair at %7", "float_frame.s",
             "seth %7,  16384");
      Check ("call: double return read from %3", "float_frame.s",
             "set %0, %3");
   end Float_Frame;

   ---------------
   -- Next_3X_1 --
   ---------------

   procedure Next_3X_1 is
      Code : Tagatha.Code.Instance;
      L1   : constant Tagatha.Code.Label := Code.Next_Label;
      L2   : constant Tagatha.Code.Label := Code.Next_Label;
   begin
      Code.Begin_Routine ("next_3x_1");

      Code.Push_Argument (1);
      Code.Push_Constant (Tagatha.Int_32'(2));
      Code.Operate (Tagatha.Op_Mod);
      Code.Branch (Tagatha.Z, L1);
      Code.Push_Argument (1);
      Code.Push_Constant (Tagatha.Int_32'(3));
      Code.Operate (Tagatha.Op_Multiply);
      Code.Push_Constant (Tagatha.Int_32'(1));
      Code.Operate (Tagatha.Op_Add);
      Code.Pop_Local (1);
      Code.Branch (L2);
      Code.Set_Label (L1);
      Code.Push_Argument (1);
      Code.Push_Constant (Tagatha.Int_32'(2));
      Code.Operate (Tagatha.Op_Divide);
      Code.Pop_Local (1);
      Code.Set_Label (L2);
      Code.Push_Local (1);
      Code.Pop_Result (1);
      Code.End_Routine;
      Code.Save ("return_x_plus_1.lst");

      declare
         Target : Tagatha.Arch.Pdp11.Instance;
      begin
         Code.Generate (Target);
         Target.Save ("next3x_1.m11");
      end;

      declare
         Target : Tagatha.Arch.M6502.Instance;
      begin
         Target.Set_Option (Tagatha.Arch.No_Recursion);
         Code.Generate (Target);
         Target.Save ("next3x_1.6502");
      end;

      declare
         Target : Tagatha.Arch.Aqua.Instance;
      begin
         Code.Generate (Target);
         Target.Save ("next3x_1.s");
      end;

      --  Every slot is one word wide here, so the layout must be unchanged
      --  by the switch to prefix sums: arg at %0, result at %1, local at %2.
      Check ("next_3x_1: integer frame unchanged", "next3x_1.s",
             "set %1, %2");
   end Next_3X_1;

   ----------------------------
   -- Unused_Float_Argument --
   ----------------------------

   procedure Unused_Float_Argument is
      --  int pick (double unused, int n) is return n; end
      --
      --  The double is declared but never touched.  Inference from accesses
      --  alone would size it as one word and put n at %1, which is not where
      --  the caller puts it -- so the declaration has to win.  n must be %2.
      Code : Tagatha.Code.Instance;
      Opts : constant Tagatha.Code.Routine_Options'Class :=
               Tagatha.Code.Set_Argument_Count (2)
               .Set_Argument_Content (1, Tagatha.Floating_Point_Content);
   begin
      Code.Begin_Routine ("pick", Opts);
      Code.Push_Argument (2);
      Code.Pop_Result (1);
      Code.Exit_Routine;
      Code.End_Routine;

      declare
         Target : Tagatha.Arch.Aqua.Instance;
      begin
         Code.Generate (Target);
         Target.Save ("unused_float_arg.s");
      end;

      --  Declared double at %0/%1, so n is %2 and the result slot is %3.
      Check ("pick: unused double still occupies a pair",
             "unused_float_arg.s", "set %3, %2");
   end Unused_Float_Argument;

begin
   Next_3X_1;
   Float_Frame;
   Unused_Float_Argument;

   Ada.Text_IO.Put_Line ("failures:" & Failures'Image);
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Tests;
