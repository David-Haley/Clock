-- Main programme of IOT Clock
-- Author    : David Haley
-- Created   : 16/07/2019
-- Last Edit : 20/09/2022
-- 20220920 : User initiated shutdown moved to main loop.
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220125 : Reporting to User Interface updated.
-- 20220123 : Units of hours decimal point indicates chiming disabled. Ambient
-- light initialisation chamgedd to AL_Driver, AL_Channel.
-- 20220119 : UI chiming state read from UI
-- 20220118 : Decoupling UI server from main loop;
-- 20220115 : Error and event handiing centralised. Primary and Secondary
-- display brightness updated when sweep updated.
-- 20191111 : The time step for which all updates are run is reduced to 3.0 s.
-- Thus if any correction in time greter than 3.0 s occurs the secondary display
-- will also be resynchronised.
-- 20190726 : Clock_Driver used. Code added to manage large steps in time
-- without requiring too many display updates.
-- 20190725 : Build with user interface
-- 20190722 : Non-linear ramp provided in Update_Sweep using gamma value read
-- from Brightness record.
-- 20190721 : Revised eight step sweep animation. exception handler added.
-- 20190720 : Version number on debug screen. Update frequency increased to 4Hz,
-- that is, every 250ms. Four step sweep animation implemented.
-- 20190719 : Clear_Screen added to Display State
-- 20190718 : debug switch added and time display added to Display_State.
-- 20190717 : Lit_Greyscales corrected and Secondary display added

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces; use Interfaces;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with RPi_GPIO; use RPi_GPIO;
with Linux_Signals; use Linux_signals;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;
with Clock_Driver; use Clock_Driver;
with Brightness; use Brightness;
with Chime; use Chime;
with Secondary_Display; use Secondary_Display;
with Shared_User_Interface; use Shared_User_Interface;
with User_Interface_Server; use User_Interface_Server;

procedure IOT_Clock is

   use Clock_LEDs;

   Update_Interval : constant Duration := 0.125; -- 125ms
   -- 8 Hz update rate, primarilly for sweep animation

   package Real_Numerics is new
     Ada.Numerics.Generic_Elementary_Functions (Real);

   use Real_Numerics;

   procedure Initialise_Hardware (Brighness_Record : in Brighness_Records) is

   begin -- Initialise_Hardware
      Bind_Pin (X_Error, In_Pin);
      for Driver in LED_Drivers loop
         for LED in LED_Channels loop
            Set_Correction (Driver, LED,
                            Brighness_Record.Dot_Correction (Driver, LED));
         end loop; -- LED in LED_Channels
      end loop; -- LED in LED_Channels
      Write_Corrections;
      for Digit in Display_Digits loop
         Initialise_Digit (Digit, Display_Array (Digit).Driver,
                           Display_Array (Digit).Segment_Array);
      end loop; -- Digit in Display_Digits
      Initialise_Ambient_Light (AL_Driver, AL_Channel, Gen4);
      Light_LEDs;
   end Initialise_Hardware;

   function Truncate_Second (T : in Time) return Time is
      -- Effectively rounds T down such that Sub_Seconds are zero

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;

   begin -- Truncate_Second
      Split (T, Year, Month, Day,
             Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Time_Offset (T));
      return Time_Of (Year, Month, Day,
                      Hour, Minute, Second, 0.0,
                      Leap_Second, UTC_Time_Offset (T));
   end Truncate_Second;

   procedure Update_Sweep (Next_Time : in Time;
                           Display_Brightness  : in Lit_Greyscales;
                           Gamma : in Gammas) is

      -- Updates all the sweep and marker LEDs. Note all the sweep leds must be
      -- updated because there is the posibility of a significant jump in time
      -- occuring as a result of an NTP update, particularly during startup or
      -- if there has been a long disruption to network connectivity.

      Blink : constant Second_Duration := 0.25;
      -- Duration markers are turned off as sweep passes by
      Steps : constant Real := 1.0 / Real (Update_Interval);
      Sweep_LEDs : array (Sweep_Indices) of Greyscales := (others => 0);
      LED_Index : Sweep_Indices;
      Tail_Brightness : Greyscales := Shift_Right (Display_Brightness, 1);

   begin -- Update_Sweep
      for Marker in Markers loop
         Set_Greyscale (Marker_Array (Marker).Driver,
                        Marker_Array (Marker).Channel, Display_Brightness);
      end loop; -- Marker in Markers
      if Second (Next_Time) = 0 and  Sub_Second (Next_Time) < Blink then
         Set_Greyscale (Marker_Array (Marker_00).Driver,
                        Marker_Array (Marker_00).Channel, Greyscales'First);
      elsif Second (Next_Time) = 15 and  Sub_Second (Next_Time) < Blink then
         Set_Greyscale (Marker_Array (Marker_15).Driver,
                        Marker_Array (Marker_15).Channel, Greyscales'First);
      elsif Second (Next_Time) = 30 and  Sub_Second (Next_Time) < Blink then
         Set_Greyscale (Marker_Array (Marker_30).Driver,
                        Marker_Array (Marker_30).Channel, Greyscales'First);
      elsif Second (Next_Time) = 45 and  Sub_Second (Next_Time) < Blink then
         Set_Greyscale (Marker_Array (Marker_45).Driver,
                        Marker_Array (Marker_45).Channel, Greyscales'First);
      end if; -- Second (Next_Time) = 0 and  Sub_Second (Next_Time) < 0.25
      LED_Index := Sweep_Indices (Second (Next_Time));
      Sweep_LEDs (LED_Index) := Display_Brightness;
      -- current second LED fully lit
      Sweep_LEDs (LED_Index + 1) :=
        Greyscales (Real'Floor (Real (Display_Brightness) *
                    (Real (Sub_Second (Next_Time)) /
                         Real (Update_Interval)/ Steps) ** Gamma));
      -- make tail
      while Tail_Brightness > 0 loop
         Sweep_LEDs (LED_Index - 1) := Tail_Brightness;
         LED_Index := LED_Index - 1;
         Tail_Brightness := Shift_Right (Tail_Brightness, 1);
      end loop; -- Tail_Brightness > 0
      for I in Sweep_Indices loop
         Set_Greyscale (Sweep_Array (I).Driver, Sweep_Array (I).Channel,
                        Sweep_LEDs (I));
      end loop; -- I in Sweep_Indices
   end Update_Sweep;

   procedure Update_Primary (Next_Time : in Time;
                             Display_Brightness : in Lit_Greyscales) is

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;

   begin -- Update_Primary
      Report_Time (Next_Time);
      -- Copy currently displayed time to user interfsce
      Split (Next_Time, Year, Month, Day,
             Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Time_Offset (Next_Time));
      Set_Digit (Tens_Hours, Hour / 10, Display_Brightness);
      Set_Digit (Units_Hours, Hour mod 10, Display_Brightness,
                 not Get_Chime_Toggle);
      -- Turn on decimal point when chiming is disabled by user control
      Set_Digit (Tens_Minutes, Minute / 10, Display_Brightness);
      Set_Digit (Units_Minutes, Minute mod 10, Display_Brightness);
      Set_Digit (Tens_Seconds, Second / 10, Display_Brightness);
      Set_Digit (Units_Seconds, Second mod 10, Display_Brightness, False, True);
      -- DP driver is assigned to automatic brightness
   end Update_Primary;

   Brighness_Record : Brighness_Records := Read_Brightness_Config;
   Next_Time : Time := Truncate_Second (Clock + 1.0) + Update_Interval;
   -- First loop execution delayed such that the loop will at 00 subseconds plus
   -- a multiple of the Update Interval and not in the past (+1s).
   Time_Step : Duration := 3.0;
   -- This is the naximum NTP correction which will allow clock to run all
   -- display updates.
   Display_Brightness : Lit_Greyscales := Brighness_Record.Minimum_Brightness;

begin -- IOT_Clock
   Handlers.Install;
   Start_Events;
   Put_Event ("IOT_Clock version " & Clock_Version & " started");
   Initialise_Hardware (Brighness_Record);
   Initialise_Secondary_Display;
   loop -- 8 Hz loop
      delay until Next_Time;
      if Clock < Next_Time - Time_Step or Clock > Next_Time + Time_Step then
         -- A large time correction has occured from NTP update or a restart.
         Resync_Secondary;
         Next_Time := Truncate_Second (Clock + 1.0) + Update_Interval;
         delay until Next_Time;
      end if; --  Clock < Next_Time - Time_Step or Clock > Next_Time + Time_Step
      Update_Sweep (Next_Time, Display_Brightness, Brighness_Record.Gamma);
      Update_Primary (Next_Time, Display_Brightness);
      if Sub_Second (Next_Time) < Update_Interval then
         -- Secondary display stepped forward once per second.
         Update_Secondary (Next_Time, Display_Brightness, True);
      else
         Update_Secondary (Next_Time, Display_Brightness);
      end if; -- Sub_Second (Next_Time) < Update_Interval
      if Get_Ambient_Light > Brighness_Record.Minimum_Brightness then
         Display_Brightness := Get_Ambient_Light;
      else
         Display_Brightness := Brighness_Record.Minimum_Brightness;
      end if; -- Get_Ambient_Light > Brighness_Record.Minimum_Brightness
      if Display_Brightness > Brighness_Record.Chime_Brightness and
        Get_Chime_Toggle then
         Chiming;
      else
         Silent;
      end if; -- Display_Brightness > Brighness_Record.Chime_Brightness and .,.
      Write_LEDs;
      -- Report status to user interface
      for D in LED_Drivers loop
         For C in LED_Channels loop
            Report_LED (D, C, Get_Greyscale (D, C));
         end loop;  -- C in LED_Channels
      end loop; -- D in LED_Drivers
      Report_Ambient_Light (Get_Ambient_Light,
                            Get_Greyscale (AL_Driver, AL_Channel));
      Next_Time := Next_Time + Update_Interval;
      exit when Ctrl_C_Stop or Handlers.Signal_Stop;
   end loop; -- 8 Hz loop
   -- stop other tasks to allow termination
   End_Chiming;
   Stop_UI_Server;
   Put_Event ("IOT_Clock stopped (user request)");
   Stop_Events;
   Handlers.Remove;
exception
   when Event: others =>
      Put_Error ("**** Unhandled Exception ****", Event);
      Stop_UI_Server;
      End_Chiming;
      Stop_Events;
      Handlers.Remove;
      -- terminate execution
end IOT_Clock;
