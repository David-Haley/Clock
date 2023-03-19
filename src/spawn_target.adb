with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Spawn_Target is

begin -- Spawn_Target
   Put_Line ("Command: <" & Command_Name & ">");
   Put_Line ("Number of Arguments:" & Natural'Image (Argument_Count));
   for I in Natural range 1 .. Argument_Count loop
      Put_Line ("Arg (" & Natural'Image (I) & "): <" & Argument (I) & ">");
   end loop; -- I in Natural range 1 .. Argument_Count)
   Set_Exit_Status (Exit_Status (Argument_Count));
end Spawn_Target;
