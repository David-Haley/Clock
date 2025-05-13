-- Main programme of IOT Clock
-- Author    : David Haley
-- Created   : 16/07/2019
-- Last Edit : 13/05/2025

-- 20250513 : Smooth added as a simulated sweep mode.
-- 20250512 : Provision for multiple simulated sweep hand modes.
-- 20250411 : Correction of Spelling of Arbitrary, reporting of all
-- configuration file modification times, Play_Command and Volume_Command now
-- read from general configuration file, Time_Zone now supports multiple changes
-- in UTC offset.
-- 20250407 : General_Configuration added, default volume now configurable.
-- Start and end dates for daylight saving added to secobdary display.
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
-- Thus if any correction in time greater than 3.0 s occurs the secondary
-- display will also be resynchronised.
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
with General_Configuration; use General_Configuration;
with Brightness; use Brightness;
with Chime; use Chime;
with Secondary_Display; use Secondary_Display;
with Shared_User_Interface; use Shared_User_Interface;
with User_Interface_Server; use User_Interface_Server;

procedure IOT_Clock is

   use Clock_LEDs;
   
   Update_Rate : constant array (Sweep_Modes) of Positive 
     := (Normal => 8, Smooth => 16, With_Tail => 8,
     Sub_Second => Sweep_Indices'Modulus);
   -- 8 Hz (Normal), 16 Hz (Smooth) 8 Hz (With_Tail) 60 Hz (Sub_Second)

   package Real_Numerics is new
     Ada.Numerics.Generic_Elementary_Functions (Real);

   use Real_Numerics;

   procedure Initialise_Hardware (Dot_Correction : in Dot_Corrections) is

   begin -- Initialise_Hardware
      Bind_Pin (X_Error, In_Pin);
      for Driver in LED_Drivers loop
         for LED in LED_Channels loop
            Set_Correction (Driver, LED, Dot_Correction (Driver, LED));
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
                           S_Mode : in sweep_Modes;
                           Gamma : in Gammas) is

      -- Updates all the sweep and marker LEDs. Note all the sweep leds must be
      -- updated because there is the posibility of a significant jump in time
      -- occuring as a result of an NTP update, particularly during startup or
      -- if there has been a long disruption to network connectivity.

      Blink : constant Second_Duration := 0.125;
      -- Duration markers are turned off as sweep passes by
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
      end if; -- Second (Next_Time) = 0 and  Sub_Second (Next_Time) < Blink
      LED_Index := Sweep_Indices (Second (Next_Time));
      Sweep_LEDs (LED_Index) := Display_Brightness;
      -- current second LED fully lit
      case S_mode is
      when Normal =>
         null;
      when Smooth =>
         Sweep_LEDs (LED_Index + 1) :=
           Greyscales (Real'Floor (Real (Display_Brightness) *
                       (Real (Sub_Second (Next_Time)) ** Gamma)));
         Sweep_LEDs (LED_Index) := @ - Sweep_LEDs (LED_Index + 1);
      when With_Tail =>
         Sweep_LEDs (LED_Index + 1) :=
           Greyscales (Real'Floor (Real (Display_Brightness) *
                       (Real (Sub_Second (Next_Time)) ** Gamma)));
         -- make tail
         while Tail_Brightness > 0 loop
            LED_Index := @ - 1;
            Sweep_LEDs (LED_Index) := Tail_Brightness;
            Tail_Brightness := Shift_Right (Tail_Brightness, 1);
         end loop; -- Tail_Brightness > 0
      when Sub_Second =>
         LED_Index := @ + Sweep_Indices (Duration (Sweep_Indices'Modulus) *
           Sub_Second (Next_Time));
         Sweep_LEDs (LED_Index) := Display_Brightness;
      end case; -- S_mode
      for I in Sweep_Indices loop
         Set_Greyscale (Sweep_Array (I).Driver, Sweep_Array (I).Channel,
                        Sweep_LEDs (I));
      end loop; -- I in Sweep_Indices
   end Update_Sweep;

   procedure Update_Primary (Next_Time : in Time;
                             Display_Brightness : in Lit_Greyscales) is

      Hour : Hour_Number :=
        Ada.Calendar.Formatting.Hour (Next_Time, UTC_Time_Offset (Next_Time));
      Minute : Minute_Number :=
        Ada.Calendar.Formatting.Minute (Next_Time, UTC_Time_Offset (Next_Time));
      Second : Second_Number := Ada.Calendar.Formatting.Second (Next_Time);

   begin -- Update_Primary
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

   Dot_Correction : Dot_Corrections := Read_Brightness_Config;
   Time_Step : constant Duration := 3.0;
   -- This is the naximum NTP correction which will allow clock to run all
   -- display updates.
   Display_Brightness : Lit_Greyscales := Minimum_Brightness;
   S_Mode : Sweep_Modes;
   Update_Interval : Duration;
   Update_Count : Positive;
   Next_Time : Time := Truncate_Second (Clock + 1.0);
   -- First loop execution delayed such that the loop will at 00 subseconds plus
   -- a multiple of the Update Interval and not in the past (+1s).

begin -- IOT_Clock
   Handlers.Install;
   Put_Event ("IOT_Clock version " & Clock_Version & " started");
   Initialise_Hardware (Dot_Correction);
   Initialise_Secondary_Display;
   loop -- One second loop
      delay until Next_Time;
      if Clock < Next_Time - Time_Step or Clock > Next_Time + Time_Step then
         -- A large time correction has occured from NTP update or a restart.
         Resync_Secondary;
         Next_Time := Truncate_Second (Clock + 1.0);
         delay until Next_Time;
      end if; --  Clock < Next_Time - Time_Step or Clock > Next_Time + Time_Step
      S_Mode := Sweep_Mode;
      Update_interval := 1.0 / Duration (Update_Rate (S_Mode));
      Update_Count := 1;
      loop -- Update loop
         Display_Brightness := Get_Ambient_Light;
         Update_Sweep (Next_Time, Display_Brightness, S_Mode, Gamma);
         Update_Primary (Next_Time, Display_Brightness);
         if Update_Count = 1 then
            -- Secondary display stepped forward once per second.
            Update_Secondary (Next_Time, Display_Brightness, True);
         else
            Update_Secondary (Next_Time, Display_Brightness);
         end if; -- Update_Count = 1
         -- Copy currently displayed time to user interfsce
         if Display_Brightness < Minimum_Brightness then
            Display_Brightness := Minimum_Brightness;
         end if; -- Display_Brightness < Minimum_Brightness
         Write_LEDs;
         -- Report status to user interface
         Report_Time (Next_Time);
         for D in LED_Drivers loop
            For C in LED_Channels loop
               Report_LED (D, C, Get_Greyscale (D, C));
            end loop;  -- C in LED_Channels
         end loop; -- D in LED_Drivers
         Report_Ambient_Light (Get_Ambient_Light,
                               Get_Greyscale (AL_Driver, AL_Channel));
         exit when Update_Count >= Update_Rate (S_Mode) - 1;
         Update_Count := @ + 1;
         Next_Time := Next_Time + Update_Interval;
         delay until Next_Time;
      end loop; -- Update loop
      exit when Ctrl_C_Stop or Handlers.Signal_Stop;
      if Display_Brightness > Minimum_Chime and Get_Chime_Toggle then
         Chiming;
      else
         Silent;
      end if; -- Display_Brightness > Chime_Brightness and Get_Chime_Toggle
      Next_Time := Truncate_Second (Next_Time + 1.0);
      -- This is done so that rounding errors will not accumulate over more than
      -- one second.
   end loop; -- One Second loop
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
