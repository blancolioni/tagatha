with Tagatha.Arch.Aqua;
with Tagatha.Arch.Pdp11;
with Tagatha.Code;

procedure Tests is
   Code   : Tagatha.Code.Instance;
   L1     : constant Tagatha.Code.Label := Code.Next_Label;
   L2     : constant Tagatha.Code.Label := Code.Next_Label;
begin
   Code.Begin_Routine ("next_3x_1");
   Code.Data_Label ("hello");
   Code.String_Constant ("Hello, world");
   Code.Push_External ("hello", Address => True);
   Code.Duplicate;
   Code.Dereference;
   Code.Pop_Local (1);
   Code.Drop;
   Code.Push_Local (1);
   Code.Call ("calculate", 1, 1);
   Code.Push_Return (1);
   Code.Pop_Local (1);
   Code.Push_Argument (1);
   Code.Push_Constant (2);
   Code.Operate (Tagatha.Op_Mod);
   Code.Branch (Tagatha.Z, L1);
   Code.Push_Argument (1);
   Code.Push_Constant (3);
   Code.Operate (Tagatha.Op_Multiply);
   Code.Push_Constant (1);
   Code.Operate (Tagatha.Op_Add);
   Code.Pop_Local (1);
   Code.Branch (L2);
   Code.Set_Label (L1);
   Code.Push_Argument (1);
   Code.Push_Constant (2);
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
      Target : Tagatha.Arch.Aqua.Instance;
   begin
      Code.Generate (Target);
      Target.Save ("next3x_1.s");
   end;

end Tests;
