-- Package to provide secondary display functionality.
-- It is assumed that Update_Secondary is called once per second with
-- Step_Display True to update the secondary display contents.
-- Author    : David Haley
-- Created   : 17/07/2019
-- Last Edit : 20/08/2022
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220126 Removed protected data and implemented reporting to
-- User_Interface_Server. Package wide variables used for parsing now passed as
-- parameters.
-- 20220122 : Corrected logical error in Update_Arbitary which would have made
-- it possible to set segments in the primary display, by referencing digits
-- belonging to the primary display. Declarations and tests changed from
-- Display_Digit to Secondary_Digits;
-- 20220120 : First_Time made a state variable of Update_Display
-- 20220118 : Decoupling diagnostics using a protected variab;e.
-- 20220115 : Step_Display added to allow brightness update without also,
-- updating Time_Remaining. Initialise_Secondary_Display added.
-- 20191111 : Local exception handeler added to Update_Date and Update_Time
-- Resync_Secondary fully restarts secondary display
-- 20191109 : Exceptopn mesages enhanced to allow better debugging of
-- secondary display commands.
-- 20190726 : Clock_Driver used
-- 20190725 : Diagnostic_Strings added as return type for Current_Item
-- 20190722 : Arbitary implemented and provision of some error handling.
-- 20190720 : Synchronises first secondary display item with 0 seconds to
-- provide consistency of display when the sum of item times is a multiple or
-- sub-multiple of 60s.
-- 20190720 : Dec displays negative numbers
-- 20190719 : Implementation of Dec, Hex, Segments and Blank.
-- 20180718 : Time_Zone implemented, Current Item provided for diagnostic
-- purposes.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers; use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with LED_Declarations; use LED_Declarations;
with Clock_Driver; use Clock_Driver;
with Shared_User_Interface; use Shared_User_Interface;
with User_Interface_Server; use User_Interface_Server;

package body Secondary_Display is

   Delimiter : constant Character := ',';
   Delimiter_Set : constant Character_Set := To_Set (Delimiter);

   type Display_Items is (DDMMYY, MMDDYY, YYMMDD, Time_Zone, Dec, Hex,
                          Arbitary, Blank);
   subtype Date_Formats is Display_Items range DDMMYY .. YYMMDD;

   subtype Item_Indices is Positive;

   package Item_Arrays is new Ada.Containers.Vectors (Item_Indices,
                                                      Unbounded_String);
   use Item_Arrays;

   use Clock_LEDs;

   File_Name : constant String := "Secondary.csv";
   Text_File : File_Type;

   Item_Array : Item_Arrays.Vector := Empty_Vector;

   procedure Initialise_Secondary_Display is

   begin -- Initialise_Secondary_Display
      if Exists (File_Name) then
         Open (Text_File, In_File, File_Name);
         while not End_Of_File (Text_File) loop
            Append (Item_Array, Get_Line (Text_File));
         end loop; -- End_Of_File (Text_File)
         Close (Text_File);
      end if; -- Exists (File_Name)
   end Initialise_Secondary_Display;

   procedure Blank is

      -- extinguish all segments in secondary display

   begin -- Blank
      for Digit in Secondary_Digits loop
         for Segment in Segments loop
            Set_Greyscale (Display_Array (Digit).Driver,
                           Display_Array (Digit).Segment_Array (Segment),
                           Greyscales'First);
         end loop; -- Segment in Segments
      end loop; -- Digit in Secondary_Digits
   end Blank;

   procedure Update_Date (Format : in Date_Formats; Current_Time : in Time;
                          Display_Brightness : in Greyscales) is

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Two_Digit_Year : Natural;

   begin -- Update_Date
      Split (Current_Time, Year, Month, Day,
             Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Time_Offset (Current_Time));
      Two_Digit_Year := Year mod 100; -- only tens and units of years
      case Format is
         when DDMMYY =>
            Set_Digit (Tens_Days, Day / 10, Display_Brightness);
            Set_Digit (Units_Days, Day mod 10, Display_Brightness, True);
            Set_Digit (Tens_Months, Month / 10, Display_Brightness);
            Set_Digit (Units_Months, Month mod 10, Display_Brightness, True);
            Set_Digit (Tens_Years, Two_Digit_Year / 10, Display_Brightness);
            Set_Digit (Units_Years, Two_Digit_Year mod 10, Display_Brightness);
         when MMDDYY =>
            Set_Digit (Tens_Days, Month / 10, Display_Brightness);
            Set_Digit (Units_Days, Month mod 10, Display_Brightness, True);
            Set_Digit (Tens_Months, Day / 10, Display_Brightness);
            Set_Digit (Units_Months, Day mod 10, Display_Brightness, True);
            Set_Digit (Tens_Years, Two_Digit_Year / 10, Display_Brightness);
            Set_Digit (Units_Years, Two_Digit_Year mod 10, Display_Brightness);
         when YYMMDD =>
            Set_Digit (Tens_Days, Two_Digit_Year / 10, Display_Brightness);
            Set_Digit (Units_Days, Two_Digit_Year mod 10, Display_Brightness,
                       True);
            Set_Digit (Tens_Months, Month / 10, Display_Brightness);
            Set_Digit (Units_Months, Month mod 10, Display_Brightness, True);
            Set_Digit (Tens_Years, Day / 10, Display_Brightness);
            Set_Digit (Units_Years, Day mod 10, Display_Brightness);
      end case; -- Format
   exception
      when Event: Others =>
         Put_Error ("Error in Date item", Event);
         raise;
   end Update_Date;

   procedure Update_Time (Current_Time : in Time;
                          Display_Brightness : in Greyscales;
                          Text : in Unbounded_String;
                          Start_At, First : in out Positive;
                          Last : in out Natural) is

      -- Text Start_At, First and last are package wide variables.

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      UTC_Offset : Time_Offset;

   begin -- Update_Time
      Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
      UTC_Offset := Time_Offset'Value (Slice (Text, First, Last));
      Split (Current_Time, Year, Month, Day,
             Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Offset);
      Set_Digit (Tens_Days, Hour / 10, Display_Brightness);
      Set_Digit (Units_Days, Hour mod 10, Display_Brightness);
      Set_Digit (Tens_Months, Minute / 10, Display_Brightness);
      Set_Digit (Units_Months, Minute mod 10, Display_Brightness);
      Set_Digit (Tens_Years, Second / 10, Display_Brightness);
      Set_Digit (Units_Years, Second mod 10, Display_Brightness);
   exception
      when Event: Others =>
         Put_Error ("Error in Time item", Event);
         raise;
   end Update_Time;

   procedure Update_Dec (Display_Brightness : in Greyscales;
                         Text : in Unbounded_String;
                         Start_At, First : in out Positive;
                         Last : in out Natural) is

      -- Text, Start_At, First and last are package wide variables.

      Base : Constant Natural := 10;
      Number : Integer range -99999 .. 999999;
      Is_Negative : Boolean;

   begin -- Update_Dec
      if Start_At > Length (Text) then
         raise Secondary_Configuration with "Missing Dec number";
      end if; -- Start_At > Length (Text)
      Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
      Number := Integer'Value (Slice (Text, First, Last));
      Is_Negative := Number < 0;
      Number := abs (Number);
      Blank;
      Set_Digit (Units_Years, Number mod Base, Display_Brightness);
      -- Display least significant digit even if it is 0.
      for Digit in reverse Secondary_Digits range Tens_Days .. Tens_Years loop
         Number := Number / Base;
         if Number > 0 then
            -- leading zeros suppressed
            Set_Digit (Digit, Number mod Base, Display_Brightness);
         elsif Is_Negative then
            Set_Greyscale (Display_Array (Digit).Driver,
                           Display_Array (Digit).Segment_Array (Segment_g),
                           Display_Brightness);
            Is_Negative := False;
            -- one negative sign only
         end if; -- Number > 0
      end loop; -- Digit in reverse Secondary_Digits range ...
   exception
      when Event: Others =>
         Put_Error ("Error in Dec item", Event);
   end Update_Dec;

   procedure Update_Hex (Display_Brightness : in Greyscales;
                         Text : in Unbounded_String;
                         Start_At, First : in out Positive;
                         Last : in out Natural) is

      -- Text, Start_At, First and last are package wide variables.

      Base : Constant Natural := 16;
      Number : Natural range 0 .. 16#FFFFFF#;
      Number_Text : Unbounded_String;

   begin -- Update_Hex
      if Start_At > Length (Text) then
         raise Secondary_Configuration with "Missing Hex number";
      end if; -- Start_At > Length (Text)
      Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
      Number_Text := Unbounded_Slice (Text, First, Last);
      Number_Text := "16#" & Trim (Number_Text, Both) & "#";
      Number := Natural'Value (To_String (Number_Text));
      Blank;
      Set_Digit (Units_Years, Number mod Base, Display_Brightness);
      -- Display least significant digit even if it is 0.
      for Digit in reverse Secondary_Digits range Tens_Days .. Tens_Years loop
         Number := Number / Base;
         if Number > 0 then
            -- leading zeros suppressed
            Set_Digit (Digit, Number mod Base, Display_Brightness);
         end if; -- Number > 0
      end loop; -- Digit in reverse Secondary_Digits range ...
   exception
      when Event: Others =>
         Put_Error ("Error in Hex item", Event);
         raise;
   end Update_Hex;

   function Is_Display_Digit (S : in String) return Boolean is

      Result : Boolean := False;

   begin -- Is_Display_Digit
      For D in Secondary_Digits loop
         Result := Result or
           Ada.Strings.Equal_Case_Insensitive (Trim (S, Both),
                                               Trim (Secondary_Digits'
                                                     Image (D), Both));
      end loop; -- D in Secondary_Digits
      return Result;
   end Is_Display_Digit;

   function Is_Segment (S : in String) return Boolean is

      Result : Boolean := False;

   begin -- Is_Segment
      For Seg in Segments loop
         Result := Result or
           Ada.Strings.Equal_Case_Insensitive (Trim (S, Both),
                                               Trim (Segments'
                                                     Image (Seg), Both));
      end loop; -- Seg in Segments
      return Result;
   end Is_Segment;

   procedure Update_Arbitary (Display_Brightness : in Greyscales;
                              Text : in Unbounded_String;
                              Start_At, First : in out Positive;
                              Last : in out Natural) is

      -- Text, Start_At, First and Last are package wide variables.

      Current_Digit : Secondary_Digits;
      Digit_Defined : Boolean := False;
      Segment : Segments;

   begin -- Update_Arbitary
      Blank;
      while Start_At < Length (text) loop
         Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
         if Is_Display_Digit (Slice (Text, First, Last)) then
            Digit_Defined := True;
            Current_Digit := Secondary_Digits'Value (Slice (Text, First, Last));
         elsif Is_Segment (Slice (Text, First, Last)) then
            if Digit_Defined then
               Segment := Segments'Value (Slice (Text, First, Last));
               Set_Greyscale (Display_Array (Current_Digit).Driver,
                              Display_Array (Current_Digit).
                                Segment_Array (Segment),
                              Display_Brightness);
            else
               raise Secondary_Configuration with "Digit undefined";
            end if; -- Digit_Defined
         else
            raise Secondary_Configuration with "Not digit nor segment";
         end if; -- Is_Display_Digit (Slice (Text, First, Last))
         Start_At := Last + 1; -- advance to next element
      end loop; -- Start_At < Length (text)
   exception
      when Event: Others =>
         Put_Error ("Error in Arbitary item", Event);
         raise;
   end Update_Arbitary;

   -- State variables of Update_Secondary and Resync_Secondary.
   Item_Index : Item_Indices := Item_Indices'First;
   Time_Remaining : Second_Number := Second_Number'First;
   Dynamic, First_Run, First_Time : Boolean := True;
   Run : Boolean := False;

   procedure Update_Secondary (Current_Time : in Time;
                               Display_Brightness : in Greyscales;
                               Step_Display : Boolean := False) is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Display_Item : Display_Items;
      Item_Duration : Second_Number;

   begin -- Update_Secondary
      Run := Run or ((Second (Current_Time) = 0 and Length (Item_Array) > 0));
      -- Syncronise first run with 0 seconds
      if Run then
         -- something to display
         if Dynamic and Step_Display and not First_Run then
            -- item to be displayed may change
            if Time_Remaining > 1 then
               -- continue with displaying the same item
               Time_Remaining := Time_Remaining - 1;
               First_Time := False;
            else
               -- select the new item
               First_Time := True;
               if Item_Index < Item_Indices (Length (Item_Array)) then
                  -- more items in list
                  Item_Index := Item_Index + 1;
               else
                  -- restart at the beginning
                  Item_Index := Item_Indices'First;
               end if; -- more items in list
            end if; --  Time_Remaining > 1
         end if; --  Dynamic and Step_Display and not First_Run
         -- parse current item
         Text := Item_Array (Item_Index);
         Report_Current_Item (Head (To_String (Text), UI_Strings'Length, ' '));
         Start_At := 1;
         Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
         Start_At := Last + 1;
         Display_Item := Display_Items'Value (Slice (Text, First, Last));
         Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
         Item_Duration := Second_Number'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         -- point to character after last digit, possible delimiter
         Dynamic := Dynamic and Item_Duration > 0;
         if First_Time then
            Time_Remaining := Item_Duration;
         end if; -- First_Time
         case Display_Item is
            when DDMMYY | MMDDYY | YYMMDD =>
               Update_Date (Display_Item, Current_Time, Display_Brightness);
            when Time_Zone =>
               Update_Time (Current_Time, Display_Brightness, Text, Start_At,
                            First, Last);
            when Dec =>
               Update_Dec (Display_Brightness, Text, Start_At, First, Last);
            when Hex =>
               Update_Hex (Display_Brightness, Text, Start_At, First, Last);
            when Arbitary =>
               Update_Arbitary (Display_Brightness, Text, Start_At, First,
                                Last);
            when Blank =>
               Blank;
         end case; -- Display_Item
         First_Run := False;
      end if; -- Run
   exception
      when Event: Others =>
         Put_Error ("Error at line :" & Item_Indices'Image (Item_Index) &
                      " Character:" & First'Img, Event);
   end Update_Secondary;

   procedure Resync_Secondary is
      -- causes secondary display to be cleared and restartes at 00 seconds.

   begin -- Resync_Secondary
      Item_Index := Item_Indices'First;
      Time_Remaining := Second_Number'First;
      Dynamic := True;
      First_Run := True;
      First_Time := True;
      Run := False;
      Blank;
   end Resync_Secondary;

end Secondary_Display;
