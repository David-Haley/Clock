with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Spawn is
   Volume_Command : constant String :=
     "/usr/bin/amixer sset Master playback 50%";
   Play_Command : constant String := "/usr/bin/aplay -q big_ben_2.wav";

   Args : Argument_List_Access;
   Exit_Status : Integer;
   Good : Boolean;

begin
   Put_Line (Volume_Command);
   -- Prepare the arguments. Splitting properly takes quotes into account.
   Args := Argument_String_To_List (Volume_Command);
   -- Spawn the command and wait for its possible completion
   Spawn
     (Program_Name => Args (Args'First).all,
      Args => Args (Args'First + 1 .. Args'Last),
      Output_File => "test_volume_out.txt",
      Success => Good,
      Return_Code => Exit_Status);
   -- Free memory
   Free (Args);
   Put_Line ("Exit Status :" & Integer'Image (Exit_Status));
   Put_Line (Play_Command);
   -- Prepare the arguments. Splitting properly takes quotes into account.
   Args := Argument_String_To_List (Play_Command);
   -- Spawn the command and wait for its possible completion
   Spawn
     (Program_Name => Args (Args'First).all,
      Args => Args (Args'First + 1 .. Args'Last),
      Output_File => "test_play_out.txt",
      Success => Good,
      Return_Code => Exit_Status);
   -- Free memory
   Free (Args);
   Put_Line ("Exit Status :" & Integer'Image (Exit_Status));
end Test_Spawn;
