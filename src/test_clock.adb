-- Test program for the clock hardware.
-- Author    : David Haley
-- Created   : 07/07/2019
-- Last Edit : 06/05/2022

-- 20250406 : updated to reflect chanhes to Brightness.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220119 : End_Ambient_Light no longer required.
-- 20190910 : Test_GPIO added for diagnostic purposes.
-- 20190726 : Use of Clock_Driver
-- 20190715 : Single character selection of 20 khz tone. Multiple corrections
-- and enhancements.
-- 20190716 : Removal of instantiation of TLC5940 to LED_Declarations.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Real_Time; use Ada.Real_Time;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with ANSI_console; use ANSI_console;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;
with Clock_Driver; use Clock_Driver;
with Brightness; use Brightness;
with RPi_GPIO; use RPi_GPIO;
with TLC5940;

procedure Test_Clock is

   use Clock_LEDs;

   task Cycle_GPIO is
      entry Start_Toggle (Test_Pin : in GPIO_Pins);
      entry Stop_Toggle;
      entry Stop_Cycle;
   end Cycle_GPIO;

   procedure Test_Sound is

      Sound_Command : String := "/usr/bin/aplay -q";
      -- full path required does not do environment search

      subtype Sound_Indices is Positive range 1 .. 16#a#;

      subtype Sound_File_Names is String (1..9);

      Sound_File_Name : array (Sound_Indices) of Sound_File_Names :=
        ("00020.wav", "00050.wav",
         "00100.wav", "00200.wav", "00500.wav",
         "01000.wav", "02000.wav", "05000.wav",
         "10000.wav", "20000.wav");

      Test_Requested : Character;

      --                      12345
      Hex_Number : String := "16#x#";

      package Sound_Indices_IO is new Ada.Text_IO.Integer_IO (Sound_Indices);

   begin -- Test_Sound
      loop -- process one test request
         Clear_Screen;
         Goto_XY (X_Pos'First, Y_Pos'First);
         Put_Line ("Sound Test Menu");
         Put_Line ("0 End Sound Tests");
         for I in Sound_Indices loop
            Sound_Indices_IO.Put (Hex_Number, I, 16);
            Put (Hex_Number (4));
            Put_Line (" Play file: " &
                        Sound_File_Name (I));
         end loop; -- I in Sound_Indices
         Put ("Sound Test? ");
         Get (Test_Requested);
         exit when Test_Requested = '0';
         declare
            Sound_Index : Sound_Indices;
            Exit_Status : Integer;
            Arguments : Argument_List_Access;
            Last : Natural;
         begin -- convert Test Request
            Hex_Number (4) := Test_Requested;
            Sound_Indices_IO.Get (Hex_Number, Sound_Index, Last);
            Arguments :=
              Argument_String_To_List (Sound_Command & " " &
                                         Sound_File_Name (Sound_Index));
            Exit_Status :=
              Spawn (Program_Name => Arguments (Arguments'First).all,
                     Args => Arguments (Arguments'First + 1 .. Arguments'Last));
         exception
            when others =>
               Goto_XY (X_Pos'First, Y_Pos'Last);
               Put ("Illegal Test Request : " & Test_Requested);
               delay 3.0;
         end; -- convert Test Request
      end loop; -- process one test request
   end Test_Sound;

   procedure Test_LED is

      Prompt_Row : constant Y_Pos := 22;
      Message_Row : constant Y_Pos := Prompt_Row + 1;
      LED_Column : constant X_Pos := 25;
      LED_Start : constant Y_Pos := 1;
      Test_Column : constant X_Pos := 50;
      Main_Base : constant Natural := Character'Pos ('a');
      Dot_Correction : Dot_Corrections := Read_Brightness_Config;
      Test_Requested : Character;
      Current_Driver : LED_Drivers;
      Current_LED : LED_Channels;

      procedure Initialialise_LEDs (Dot_Correction : in Dot_Corrections) is

      begin -- Initialialise_LEDs
         Blank_LEDs;
         for Current_Driver in LED_Drivers loop
            for Current_LED in LED_Channels loop
               Set_Correction (Current_Driver, Current_LED,
                               Dot_Correction (Current_Driver, Current_LED));
               Set_Greyscale (Current_Driver, Current_LED, 0);
            end loop; -- Current_LED in LED_Channels
         end loop; --Current_Driver in LED_Drivers
         Write_Corrections;
         Write_LEDs;
         Light_LEDs;
      end Initialialise_LEDs;

      procedure Display_LED (Current_Driver : in LED_Drivers;
                             Current_LED : in LED_Channels) is

         --                      12345
         Hex_Number : String := "16#x#";

      begin -- Display_LED
         Goto_XY (LED_Column, LED_Start + Y_Pos (Current_LED));
         LED_Channel_IO.Put (Hex_Number, Current_LED, 16);
         Put (Hex_Number (4) & " (");
         Correction_IO.Put (Get_Correction (Current_Driver, Current_LED), 2);
         Put (',');
         Greyscale_IO.Put (Get_Greyscale (Current_Driver, Current_LED), 5);
         Put (')');
      end Display_LED;

      procedure Get_Correction (Current_Driver : in LED_Drivers;
                                Current_LED : in LED_Channels;
                                Dot_Correction : in out Dot_Corrections) is

         Correction : Corrections;

      begin -- Get_Correction
         Goto_XY (Test_Column, Prompt_Row);
         for X in X_Pos range Test_Column .. X_Pos'Last loop
            Put (' ');
         end loop; -- X in X_Pos range Test_Column .. X_Pos'Last
         Goto_XY (Test_Column, Prompt_Row);
         Put ("Correction? ");
         Correction_IO.Get (Correction);
         Dot_Correction (Current_Driver, Current_LED) := Correction;
         Set_Correction (Current_Driver, Current_LED, Correction);
         Write_Corrections;
         Display_LED (Current_Driver, Current_LED);
      exception
         when others =>
            null; -- unchanged
      end Get_Correction;

      function Get_Test (Prompt_X : in X_Pos;
                         Prompt_Text : in String := "Test? ") return Character is

         Result : Character;

      begin -- Get_Test
         Goto_XY (Prompt_X, Prompt_Row);
         for X in X_Pos range Prompt_X .. X_Pos'Last loop
            Put (' ');
         end loop; -- X in X_Pos range Prompt_X .. X_Pos'Last
         Goto_XY (Prompt_X, Prompt_Row);
         Put (Prompt_Text);
         Get (Result);
         return Result;
      end Get_Test;

      procedure Put_Message ( Message_X : in X_Pos; Message_Text : in String) is

      begin -- Put_Message
         Goto_XY (Message_X, Message_Row);
         for X in X_Pos range Message_X .. X_Pos'Last loop
            Put (' ');
         end loop; -- X in X_Pos range Prompt_X .. X_Pos'Last
         Goto_XY (Message_X, Message_Row);
         Put (Message_Text);
      end Put_Message;

   begin -- Test_LED
      Initialialise_LEDs (Dot_Correction);
      loop -- Driver Selection
         Clear_Screen;
         Goto_XY (X_Pos'First, Y_Pos'First);
         Put_Line ("LED Test Menu");
         for I in LED_Drivers loop
            Put_Line (Character'Val (Main_Base + LED_Drivers'Pos (I)) & " " &
                        LED_Drivers'Image(I));
         end loop; -- I in LED_Drivers
         Put_Line ("q End LED Tests");
         Test_Requested := Get_Test (X_Pos'First, "Driver? ");
         exit when Test_Requested = 'q';
         if Test_Requested >=
           Character'Val (Main_Base + LED_Drivers'Pos (LED_Drivers'First)) and
           Test_Requested <=
             Character'Val (Main_Base + LED_Drivers'Pos (LED_Drivers'Last)) then
            Current_Driver :=
              LED_Drivers'Val (Character'Pos (Test_Requested) - Main_Base);
            Put_Message (X_Pos'First, "Test Driver: " &
                           LED_Drivers'Image (Current_Driver));
            for I in LED_Channels loop
               Display_LED (Current_Driver, I);
            end loop; -- I in LED_Channels
            Goto_XY (LED_Column, LED_Start + Y_Pos (LED_Channels'Last) + 1);
            Put ("q quit LED tests");
            loop -- LED Channel Selection
               Test_Requested := Get_Test (LED_Column, "LED? ");
               exit when Test_Requested = 'q' or Test_Requested = 'Q';
               if Is_In (Test_Requested, Hexadecimal_Digit_Set) then
                  Current_Led := LED_Channels'Value ("16#" & Test_Requested &
                                                       "#");
                  Put_Message (LED_Column, "LED: " & Test_Requested);
                  Goto_XY (Test_Column, 1);
                  Put ("1 LED Lit (GS = 4095)");
                  Goto_XY (Test_Column, 2);
                  Put ("0 LED Extinguished (GS = 0)");
                  Goto_XY (Test_Column, 3);
                  Put ("c Set Correction");
                  Goto_XY (Test_Column, 4);
                  Put ("u Update Buightness");
                  Goto_XY (Test_Column, 5);
                  Put ("s select LED");
                  loop -- actual test
                     Test_Requested := Get_Test (Test_Column);
                     Put_Message (Test_Column, "Last Request: " &
                                    Test_Requested);
                     case Test_Requested is
                        when 's' | 'S' =>
                           exit;
                           when '1' =>
                           Set_Greyscale (Current_Driver, Current_LED, 4095);
                           Write_LEDs;
                        when '0' =>
                           Set_Greyscale (Current_Driver, Current_LED, 0);
                           Write_LEDs;
                        when 'c' | 'C' =>
                           Get_Correction (Current_Driver, Current_LED,
                                           Dot_Correction);
                        when 'u' | 'U' =>
                           Write_Brightness_Config (Dot_Correction);
                        when others =>
                           Put_Message (Test_Column, "Invalid Request: " &
                                          Test_Requested);
                     end case; -- Test_Requested
                     Display_LED (Current_Driver, Current_LED);
                  end loop; -- actual test
               end if; --  Is_In (Test_Requested, Hexadecimal_Digit_Set)
            end loop; -- LED Channel Selection
         end if; -- Valid dricer request
      end loop; -- Driver Selection
   end Test_LED;

   Task body Cycle_GPIO is

      Toggle_Time : constant Time_Span := Milliseconds (2);
      Toggling : Boolean := False;
      Run_Cycle : Boolean := True;
      Saved_Output : GPIO_Pins;
      Next_Time : Time := Clock + Toggle_Time;

   begin -- Cycle_GPIO
      while Run_Cycle loop
         select
            accept Start_Toggle (Test_Pin : in GPIO_Pins) do
               Toggling := True;
               Saved_Output := Test_Pin;
            end Start_Toggle;
         or
            accept Stop_Toggle do
               if Toggling then
                  Toggling := False;
                  Write_Pin (Pin_Low, Saved_Output);
               end if; -- Toggling
            end Stop_Toggle;
         or
            accept Stop_Cycle  do
               Run_Cycle := False;
            end Stop_Cycle;
         or
            delay until Next_Time;
            Next_Time := Next_Time + Toggle_Time;
            if Toggling then
               Write_Pin (not Read_Pin (Saved_Output), Saved_Output);
            end if; -- Toggling
         end select;
      end loop; -- Run_Cycle
   end Cycle_GPIO;

   procedure Test_GPIO is

      -- N.B. if the allocation of GPIO bits used to instantiate TLC5904 changes
      -- in Clock_Driver than the this procedure will also need to change to
      -- match.

      Test_Requested : Character;

   begin -- Test_GPIO
      loop -- process one test request
         Clear_Screen;
         Goto_XY (X_Pos'First, Y_Pos'First);
         Put_Line ("GPIO Test Menu");
         Put_Line ("B Blank");
         Put_Line ("V VPrg");
         Put_Line ("X XLATE");
         Put_Line ("Q End GPIO Tests");
         Put ("GPIO Test? ");
         Get (Test_Requested);
         Cycle_GPIO.Stop_Toggle;
         exit when Test_Requested = 'q' or  Test_Requested = 'Q';
         case Test_Requested is
            when 'b' | 'B' =>
               Cycle_GPIO.Start_Toggle (Gen2);
            when 'v' | 'V' =>
               Cycle_GPIO.Start_Toggle (Gen0);
            when 'x' | 'X' =>
               Cycle_GPIO.Start_Toggle (Gen1);
            when others =>
               Goto_XY (X_Pos'First, Y_Pos'Last);
               Put ("Illegal Test Request : " & Test_Requested);
               delay 3.0;
         end case; -- Test_Requested
      end loop; -- process one test request
   end Test_GPIO;

   Test_Requested : Character := ' ';

begin -- Test_Clock
   Bind_Pin (X_Error, In_Pin);
   loop -- one test
      Clear_Screen;
      Goto_XY (X_Pos'First, Y_Pos'First);
      Put_Line ("Clock Hardware Test version 20250406 (Main Menu)");
      Put_Line ("0: End Tests");
      Put_line ("1: Test Sound");
      Put_line ("2: Test LEDs");
      Put_line ("3: Test GPOI");
      Put ("Test? ");
      Get (Test_Requested);
      case Test_Requested is
         when '0' =>
            exit;
            when '1' =>
            Test_Sound;
         when '2' =>
            Test_LED;
         when '3' =>
            Test_GPIO;
         when others =>
            Goto_XY (X_Pos'First, Y_Pos'Last);
            Put ("Illegal Test Request : " & Test_Requested);
            delay 3.0;
      end case; -- Test_Requested
   end loop; -- one test
   Cycle_GPIO.Stop_Cycle;
end Test_Clock;
