-- this package declares the LED drivers and individual IO
-- Author    : David Haley
-- Created   : 28/06/2019
-- Last Edit : 13/05/2025

-- 20250513 : Smooth added as a simulated sweep mode.
-- 20250512 : Provision for multiple simulated sweep hand modes.
-- 20250412 : Final build implemention Software Requiremente 20250408.
-- 20250411 : Correction of Spelling of Arbitrary, reporting of all
-- configuration file modification times, Play_Command and Volume_Command now
-- read from general configuration file, Time_Zone now supports multiple changes
-- in UTC offset.
-- 20250407 : General_Configuration added, default volume now configurable.
-- Start and end dates for daylight saving added to secodary display.
-- 20230319 : Name of volume control changed from Headphones to PCM, required
-- as a result of Pi OS update.
-- 20220922 : UI_Server termination mechanism changed.
-- 20220920 :  User initiated shutdown moved to main loop.
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220127 : Server unconditionally replies in the event of a version mismatch
-- so that the version is reported to the client. Ambient light settling time
-- increased to 1.424 s, as a result of calculations based on NSL32 datasheet.
-- 20220126 : UI processing efficiency improved.
-- 20220123 : Units of hours decimal point indicates chiming disabled. LED
-- output used for auto brightness system declared;
-- 20220122 : Corrected logical error inUpdate_Arbitary procedure of
--  Secondary_Display package.
-- 20220116 : Additional chiming controls implemented. Error reporting
-- centralised.
-- 20191111 : Additional exception handelers in Secondary_Display and Step size
-- for all updates to run reduced to 3.0 s.
-- 20191109 : Exception handeling in Secondary_Display improved
-- 20190725 : Clock_LEDs instantiated removed to Clock_Driver to allow
-- LED_Declarations to be used fithout instamtiation TLC5940
-- 20190734 : clock version added
-- 20190715 : Segment declaretions corrected 6 for swgment g.
-- 20190716 : Instantion of Clock_LEDs moved here. Digit_Record added

With Ada.Text_IO; use Ada.Text_IO;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;

package LED_Declarations is

   subtype Version_String is String (1 .. 8);
   Clock_Version : constant Version_String := "20250513";

   type LED_Drivers is (Sweep_00_14, Sweep_15_29, Sweep_30_44, Sweep_45_59,
                        Seconds_Drv, Minutes_Drv, Hours_Drv,
                        Years_Drv, Months_Drv, Days_Drv);

   type LEDs is record
      Driver : LED_Drivers;
      Channel : LED_Channels;
   end record; -- LEDs;

   type Markers is (Marker_00, Marker_15, Marker_30, Marker_45);

   type Marker_Arrays is array (Markers) of LEDs;

   type Sweep_Indices is mod 60;

   type Sweep_Arrays is array (Sweep_Indices) of LEDs;

   type Display_Digits is (Tens_Hours, Units_Hours, Tens_Minutes, Units_Minutes,
                           Tens_Seconds, Units_Seconds, Tens_Days, Units_Days,
                           Tens_Months, Units_Months, Tens_Years, Units_Years);

   subtype Primary_Digits is Display_Digits range Tens_Hours .. Units_Seconds;

   subtype Secondary_Digits is Display_Digits range Tens_Days .. Units_Years;

   type Digit_Record is record
      Driver : LED_Drivers;
      Segment_Array : Segment_Arrays;
   end record; -- Digit_Record

   type Display_Arrays is array (Display_Digits) of Digit_Record;

   Marker_Array : constant Marker_Arrays
     := (Marker_00 => (Sweep_45_59, 15),
         Marker_15 => (Sweep_00_14, 15),
         Marker_30 => (Sweep_15_29, 15),
         Marker_45 => (Sweep_30_44, 15));

   Sweep_Array : constant Sweep_Arrays
     := (00 => (Sweep_00_14, 00), 01 => (Sweep_00_14, 01),
         02 => (Sweep_00_14, 02), 03 => (Sweep_00_14, 03),
         04 => (Sweep_00_14, 04), 05 => (Sweep_00_14, 05),
         06 => (Sweep_00_14, 06), 07 => (Sweep_00_14, 07),
         08 => (Sweep_00_14, 08), 09 => (Sweep_00_14, 09),
         10 => (Sweep_00_14, 10), 11 => (Sweep_00_14, 11),
         12 => (Sweep_00_14, 12), 13 => (Sweep_00_14, 13),
         14 => (Sweep_00_14, 14),
         15 => (Sweep_15_29, 00), 16 => (Sweep_15_29, 01),
         17 => (Sweep_15_29, 02), 18 => (Sweep_15_29, 03),
         19 => (Sweep_15_29, 04), 20 => (Sweep_15_29, 05),
         21 => (Sweep_15_29, 06), 22 => (Sweep_15_29, 07),
         23 => (Sweep_15_29, 08), 24 => (Sweep_15_29, 09),
         25 => (Sweep_15_29, 10), 26 => (Sweep_15_29, 11),
         27 => (Sweep_15_29, 12), 28 => (Sweep_15_29, 13),
         29 => (Sweep_15_29, 14),
         30 => (Sweep_30_44, 00), 31 => (Sweep_30_44, 01),
         32 => (Sweep_30_44, 02), 33 => (Sweep_30_44, 03),
         34 => (Sweep_30_44, 04), 35 => (Sweep_30_44, 05),
         36 => (Sweep_30_44, 06), 37 => (Sweep_30_44, 07),
         38 => (Sweep_30_44, 08), 39 => (Sweep_30_44, 09),
         40 => (Sweep_30_44, 10), 41 => (Sweep_30_44, 11),
         42 => (Sweep_30_44, 12), 43 => (Sweep_30_44, 13),
         44 => (Sweep_30_44, 14),
         45 => (Sweep_45_59, 00), 46 => (Sweep_45_59, 01),
         47 => (Sweep_45_59, 02), 48 => (Sweep_45_59, 03),
         49 => (Sweep_45_59, 04), 50 => (Sweep_45_59, 05),
         51 => (Sweep_45_59, 06), 52 => (Sweep_45_59, 07),
         53 => (Sweep_45_59, 08), 54 => (Sweep_45_59, 09),
         55 => (Sweep_45_59, 10), 56 => (Sweep_45_59, 11),
         57 => (Sweep_45_59, 12), 58 => (Sweep_45_59, 13),
         59 => (Sweep_45_59, 14));

   Display_Array : constant Display_Arrays :=
     (Tens_Hours => (Hours_Drv, (0, 1, 2, 3, 4, 5, 6, 7)),
      Units_Hours => (Hours_Drv, (8, 9, 10, 11, 12, 13, 14, 15)),
      Tens_Minutes => (Minutes_Drv, (0, 1, 2, 3, 4, 5, 6, 7)),
      Units_Minutes => (Minutes_Drv, (8, 9, 10, 11, 12, 13, 14, 15)),
      Tens_Seconds => (Seconds_Drv, (0, 1, 2, 3, 4, 5, 6, 7)),
      Units_Seconds => (Seconds_Drv, (8, 9, 10, 11, 12, 13, 14, 15)),
      -- n.b. The DP is not connected, output 15 is used by the auto
      -- brightness system.
      Tens_Days => (Days_Drv, (8, 9, 10, 11, 12, 13, 14, 15)),
      Units_Days => (Days_Drv, (0, 1, 2, 3, 4, 5, 6, 7)),
      Tens_Months => (Months_Drv, (8, 9, 10, 11, 12, 13, 14, 15)),
      Units_Months => (Months_Drv, (0, 1, 2, 3, 4, 5, 6, 7)),
      Tens_Years =>  (Years_Drv, (8, 9, 10, 11, 12, 13, 14, 15)),
      Units_Years => (Years_Drv, (0, 1, 2, 3, 4, 5, 6, 7)));

   AL_Driver : constant LED_Drivers := Seconds_Drv;
   AL_Channel : constant LED_Channels := 15;
   -- LED output used for auto brightness system.

   package LED_Driver_IO is new Ada.Text_IO.Enumeration_IO (LED_Drivers);

end LED_Declarations;
