-- Package to provide secondary display functionality.
-- It is assumed that Update_Secondary is called once per second with
-- Step_Display True to update the secondary display contents.
-- Author    : David Haley
-- Created   : 17/07/2019
-- Last Edit : 12/04/2025

--  20260412 : Limiting the number of exceptions raised due to parsing errors.
--  Once an item raises an exception it is not reparsed.
--  20260411 : Error management in Update_Time improved. Static_Text added.
-- 20250512 : Changes to ensure that every time Update_Secondary is called the
-- display buffer is rewritten. Correction of a possible flaw, removal of the
-- Secondary.csv file could cause an exception when the end of the list is
-- reached.
-- 20250412 : Final build for Software Requirements 20250408 various comments
-- corrected.
-- 20250411 : Correction to daylight saving logic.
-- 20250408 : Support for multiple start and end times for daylight saving
-- added. Automatic reloading of the secondary display added and conversion from
-- a vector to a list of display items. Correction of the spelling of Arbitrary
-- which will require the correction of the Python script build_secondary.
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220126 Removed protected data and implemented reporting to
-- User_Interface_Server. Package wide variables used for parsing now passed as
-- parameters.
-- 20220122 : Corrected logical error in Update_Arbitrary which would have made
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
-- 20190722 : Arbitrary implemented and provision of some error handling.
-- 20190720 : Synchronises first secondary display item with 0 seconds to
-- provide consistency of display when the sum of item times is a multiple or
-- sub-multiple of 60s.
-- 20190720 : Dec displays negative numbers
-- 20190719 : Implementation of Dec, Hex, Segments and Blank.
-- 20180718 : Time_Zone implemented, Current Item provided for diagnostic
-- purposes.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with LED_Declarations; use LED_Declarations;
with Clock_Driver; use Clock_Driver;
with Shared_User_Interface; use Shared_User_Interface;
with User_Interface_Server; use User_Interface_Server;

package body Secondary_Display is

   subtype Decimal_Digit is Natural range 0 .. 9;

   Delimiter : constant Character := ',';
   Delimiter_Set : constant Character_Set := To_Set (Delimiter);
   Signed_Number_Set : constant Character_Set :=
     Decimal_Digit_Set or To_Set ('-');

   type Display_Items is (DDMMYY, MMDDYY, YYMMDD, Time_Zone, Static_Text, 
                          Scrolling_Text, Arbitrary, Blank);
   subtype Date_Formats is Display_Items range DDMMYY .. YYMMDD;

   subtype Duration_Counters is Natural range 0 .. 3600;
   subtype Item_Durations is Duration_Counters range 1 .. Duration_Counters'Last;

   use Clock_LEDs;

   function To_Character (Number : in Decimal_Digit) return Character is
      (Character'Val (Character'Pos ('0') + Number));

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
            Set_Character (Tens_Days, To_Character (Day / 10),
                       Display_Brightness);
            Set_Character (Units_Days, To_Character (Day mod 10),
                       Display_Brightness, True);
            Set_Character (Tens_Months, To_Character (Month / 10),
                       Display_Brightness);
            Set_Character (Units_Months, To_Character (Month mod 10),
                       Display_Brightness, True);
            Set_Character (Tens_Years, To_Character (Two_Digit_Year / 10),
                       Display_Brightness);
            Set_Character (Units_Years, To_Character (Two_Digit_Year mod 10),
                       Display_Brightness);
         when MMDDYY =>
            Set_Character (Tens_Months, To_Character (Day / 10),
                       Display_Brightness);
            Set_Character (Units_Months, To_Character (Day mod 10),
                       Display_Brightness, True);
            Set_Character (Tens_Days, To_Character (Month / 10),
                       Display_Brightness);
            Set_Character (Units_Days, To_Character (Month mod 10),
                       Display_Brightness, True);
            Set_Character (Tens_Years, To_Character (Two_Digit_Year / 10),
                       Display_Brightness);
            Set_Character (Units_Years, To_Character (Two_Digit_Year mod 10),
                       Display_Brightness);
         when YYMMDD =>
            Set_Character (Tens_Years, To_Character (Day / 10),
                       Display_Brightness);
            Set_Character (Units_Years, To_Character (Day mod 10),
                       Display_Brightness, True);
            Set_Character (Tens_Months, To_Character (Month / 10),
                       Display_Brightness);
            Set_Character (Units_Months, To_Character (Month mod 10),
                       Display_Brightness, True);
            Set_Character (Tens_Days, To_Character (Two_Digit_Year / 10),
                       Display_Brightness);
            Set_Character (Units_Days, To_Character (Two_Digit_Year mod 10),
                       Display_Brightness);
      end case; -- Format
   exception
      when Event: Others =>
         Put_Error ("Error in Date item " & Format'Img & " - ", Event);
         raise;
   end Update_Date;

   procedure Update_Time (Current_Time : in Time;
                          Display_Brightness : in Greyscales;
                          Text : in Unbounded_String;
                          Start_At, First : in out Positive;
                          Last : in out Natural) is

      --  Text Start_At, First and Last are package wide variables.

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Offset, Offset_from_UTC : Time_Offset;
      Start_Time : Time;
      Previous_Time : Time := Value ("2019-01-01 00:00:00");
      -- A time before the hardware existed!
      Defined : Boolean := False;

   begin -- Update_Time
      -- Allows for mutiple start dates and associated offsets to be read
      loop -- until end of line
         Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
         if Last = 0 then
            raise  Secondary_Configuration with "Missing date and time";
         end if; -- Last = 0 then
         Start_Time := Value (Trim (Slice (Text, First, Last), Left), 0);
         -- Trim required to remove leading spaces, if any, which are not
         -- accepted by Value!
         Start_At := Last + 1;
         Find_Token (Text, Signed_Number_Set, Start_At, Inside, First, Last);
         Offset := Time_Offset'Value (Slice (Text, First, Last));
         if Last = 0 then
            raise  Secondary_Configuration with "Missing offset";
         end if; -- Last = 0 then
         Start_At := Last + 1;
         -- Only Defined if a valid Start_Time and Offset have been read where
         -- Current_Time (UTC) is after Start_Time and later that any previously
         -- read Start_Date. Note the whole line has to be valid for a time to
         -- be displayed.
         if Start_Time > Previous_Time and Current_Time > Start_Time then
            Defined := True;
            Offset_from_UTC := Offset;
            Previous_Time := Start_Time;
         end if; -- Start_Time > Previous_Time and Current_Time > Start_Time
         exit when Last >= Length (Text);
      end loop; -- until end of line
      if Defined then
         Split (Current_Time, Year, Month, Day,
                Hour, Minute, Second, Sub_Second,
                Leap_Second, Offset_from_UTC);
         Set_Character (Tens_Days, To_Character (Hour / 10),
                    Display_Brightness);
         Set_Character (Units_Days, To_Character (Hour mod 10),
                    Display_Brightness);
         Set_Character (Tens_Months, To_Character (Minute / 10),
                    Display_Brightness);
         Set_Character (Units_Months, To_Character (Minute mod 10),
                    Display_Brightness);
         Set_Character (Tens_Years, To_Character (Second / 10),
                    Display_Brightness);
         Set_Character (Units_Years, To_Character (Second mod 10),
                    Display_Brightness);
      else
         Blank;
      end if; -- Defined
   exception
      when Event: Others =>
         Blank; 
         Put_Error ("Error in Time item", Event);
         raise;
   end Update_Time;

   procedure Update_Static_Text (Display_Brightness : in Greyscales;
                                 Text : in Unbounded_String;
                                 Start_At, First : in out Positive) is

      --  Text, and First are package wide variables.
      
      Char_Position : Secondary_Digits := Secondary_Digits'First;

   begin -- Update_Static_Text
      Blank; -- Initialisation and default if an exception is raised.
      First := Start_At;
      if First + 1 > Length (Text) then
         raise Secondary_Configuration with "Missing text";
      end if; -- First + 1 > Length (Text)
      if Is_In (Element (Text, First), Delimiter_Set) then
         First := @ + 1; -- Skip the delimiter
      else
         raise Secondary_Configuration with "Expected ',' and found '" & 
           Element (Text, First) & ''';
      end if; -- Is_In (Element (Text, First), Delimiter_Set)
      loop -- Set one character
         if Is_Alphanumeric (Element (Text, First)) or
           Is_Space (Element (Text, First))
         then
            if First < Length (Text) and then
              Element (Text, First + 1) = '.'
            then
               Set_Character (Char_Position, Element (Text, First),
                              Display_Brightness, True);
               First := @ + 2;
            else
               Set_Character (Char_Position, Element (Text, First),
                              Display_Brightness);
               First := @ + 1;
            end if; -- First < Length (Text) and then ...Char_Position
         else
            Set_Character (Char_Position, Element (Text, First),
                           Display_Brightness);
            First := @ + 1;
         end if; -- Is_Alphanumeric (Element (Text, First)) or ...
         exit when Char_Position = Secondary_Digits'Last or
           First > Length (Text);
         Char_Position := Display_Digits'Succ (Char_Position);
      end loop; -- Set one character
   exception
      when Event: Others =>
         Put_Error ("Error in Static_Text item", Event);
         raise;
   end Update_Static_Text;

   procedure Update_Scrolling_Text (Display_Brightness : in Greyscales;
                                    Text : in Unbounded_String;
                                    First : in out Positive;
                                    Text_Display_Start : in Positive;
                                    Display_Start : in Secondary_Digits) is

      --  Text and First are package wide variables
      --  Assuming the logic is correct, it should not be possible to raise an
      --  exception in this procedure, due to an error in the confifuration
      --  file.  
      
      Current : Positive := Text_Display_Start;
      Char_Position : Secondary_Digits := Display_Start;

   begin -- Update_Scrolling_Text
      loop -- Set one character
         First := Current; -- For error messaging
         if Is_Alphanumeric (Element (Text, Current)) or
           Is_Space (Element (Text, Current))
         then
            if Length (Text) > Current and then
              Element (Text, Current + 1) = '.'
            then
               Set_Character (Char_Position, Element (Text, Current),
                              Display_Brightness, True);
               Current := @ + 2;
            else
               Set_Character (Char_Position, Element (Text, Current),
                              Display_Brightness);
               Current := @ + 1;
            end if; -- Length (Text) > Current and then ...
         else
            Set_Character (Char_Position, Element (Text, Current),
                           Display_Brightness);
            Current := @ + 1;
         end if; -- Is_Alphanumeric (Element (Text, Current)) or ...
         exit when Char_Position = Secondary_Digits'Last or
           Current > Length (Text);
         Char_Position := Display_Digits'Succ (Char_Position);
      end loop; -- Update_Scrolling_Text
   exception
      when Event: Others =>
         Put_Error ("Error in Scrolling_Text item", Event);
         raise;
   end Update_Scrolling_Text;

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

   procedure Update_Arbitrary (Display_Brightness : in Greyscales;
                               Text : in Unbounded_String;
                               Start_At, First : in out Positive;
                               Last : in out Natural) is

      -- Text, Start_At, First and Last are package wide variables.

      Current_Digit : Secondary_Digits;
      Digit_Defined : Boolean := False;
      Segment : Segments;

   begin -- Update_Arbitrary
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
         Put_Error ("Error in Arbitrary item", Event);
         raise;
   end Update_Arbitrary;

   type Items is record
      Valid : Boolean := True;
      Item_Number : Positive_Count := Positive_Count'First;
      Text : Unbounded_String := Null_Unbounded_String;
   end record; -- Items

   package Item_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Items);
   use Item_Lists;

   -- State variables of Update_Secondary and Resync_Secondary.
   File_Name : constant String := "Secondary.csv";
   File_Time : Time;
   Item_List : Item_Lists.List := Empty_List;
   Item_Cursor : Item_Lists.Cursor;
   Time_Remaining : Duration_Counters := Duration_Counters'First;
   First_Run, First_Time : Boolean := True;
   Run, File_Read : Boolean := False;
   Text_Display_Start : Positive;
   --  First character of scrolling text to be displayed.
   Display_Start : Secondary_Digits;
   --  Display where the first character of scrolling text is placed.

   procedure Resync_Secondary is
      -- causes secondary display to be cleared and restartes at 00 seconds.

   begin -- Resync_Secondary
      Item_Cursor := Item_Lists.First (Item_List);
      Time_Remaining := Duration_Counters'First;
      First_Run := True;
      First_Time := True;
      Run := False;
      Blank;
      Report_Current_Item (Head ("Blank - Resync_Secondary",
                                 UI_Strings'Length));
   end Resync_Secondary;

   procedure Initialise_Secondary_Display is

      Text_File : File_Type;
      Item : Items;

   begin -- Initialise_Secondary_Display
      begin -- File open exception block
         Open (Text_File, In_File, File_Name);
      exception
         when others =>
            null;
      end; -- File open exception block
      -- Rather than simply testing that the file exists opening the file shows
      -- that it is exists and is not locked due to being currently edited.
      if Is_Open (Text_File) then
         Clear (Item_List);
         while not End_Of_File (Text_File) loop
            Item.Item_Number := Line(Text_File);
            Item.Text :=  Get_Line (Text_File);
            Append (Item_List, Item);
         end loop; -- End_Of_File (Text_File)
         File_Time := Modification_Time (File_Name);
         Put_Event ("Read " & File_Name & " file time " &
                    Local_Image (Modification_Time (File_Name)));
         Close (Text_File);
         Resync_Secondary;
      else
         Blank;
         Report_Current_Item (Head ("Blank - Initialise_Secondary_Display",
                                    UI_Strings'Length));
      end if; -- Is_Open (Text_File)
   end Initialise_Secondary_Display;

   procedure Update_Secondary (Current_Time : in Time;
                               Display_Brightness : in Greyscales;
                               Step_Display : Boolean := False) is

      Start_At, First : Positive;
      Last : Natural;
      Display_Item : Display_Items;
      Item_Duration : Item_Durations;

   begin -- Update_Secondary
      Run := (Run or Second (Current_Time) = 0) and not Is_Empty (Item_List);
      -- Synchronise first run with 0 seconds, stop if the list ie empty or
      -- becomes empty.
      if Run then
         -- something to display
         if Step_Display and not First_Run then
            -- item to be displayed may change
            if Time_Remaining > 1 then
               -- continue with displaying the same item
               Time_Remaining := Time_Remaining - 1;
               First_Time := False;
            else
               -- Select the next item to be displayed item.
               First_Time := True;
               loop -- get next valid Item
                  Next (Item_Cursor);
                  exit when Item_Cursor = Item_Lists.No_Element or else 
                    Element (Item_Cursor).Valid;
               end loop; -- get next valid Item
               if Item_Cursor = Item_Lists.No_Element then
                  --  End of list has been reached
                  if not Exists (File_Name) or else
                    File_Time /= Modification_Time (File_Name) then
                     -- The secondary file has been removed, replaced or
                     -- modified.
                     Initialise_Secondary_Display;
                  end if; -- File_Time /= Modification_Time (File_Name)
                  Item_Cursor := Item_Lists.First (Item_List);
                  --  Allow for the first item to be invalid or an empty list.
                  while Item_Cursor /= Item_Lists.No_Element and then
                    not Element (Item_Cursor).Valid
                  loop
                     Next (Item_Cursor);
                  end loop; -- Item_Cursor /= Item_Lists.No_Element and ...
               end if; -- Item_Cursor = Item_Lists.No_Element
            end if; --  Time_Remaining > 1
         end if; -- Step_Display and not First_Run
         if Item_Cursor /= Item_Lists.No_Element and then
           Element (Item_Cursor).Valid
         then
            --  Parse current item.
            Report_Current_Item (Head (To_String (Element (Item_Cursor).Text),
                                 UI_Strings'Length, ' '));
            Start_At := 1;
            Find_Token (Element (Item_Cursor).Text, Delimiter_Set, Start_At,
                        Outside, First, Last);
            Start_At := Last + 1;
            Display_Item :=
              Display_Items'Value (Slice (Element (Item_Cursor).Text, First,
                                          Last));
            Find_Token (Element (Item_Cursor).Text, Delimiter_Set, Start_At,
                        Outside, First, Last);
            Item_Duration :=
              Item_Durations'Value (Slice (Element (Item_Cursor).Text, First,
                                           Last));
            Start_At := Last + 1;
            -- point to character after last digit, possible delimiter
            if First_Time then
               Time_Remaining := Item_Duration;
            end if; -- First_Time
            case Display_Item is
               when DDMMYY | MMDDYY | YYMMDD =>
                  Update_Date (Display_Item, Current_Time, Display_Brightness);
               when Time_Zone =>
                  Update_Time (Current_Time, Display_Brightness,
                               Element (Item_Cursor).Text, Start_At, First,
                               Last);
               when Static_Text =>
                  Update_Static_Text (Display_Brightness,
                                      Element (Item_Cursor).Text, Start_At,
                                      First);
               when Scrolling_Text =>
                  --  Initialisation and default if an exception is raised.
                  Blank;
                  if First_Time then
                     First := Start_At;
                     if First > Length (Element (Item_Cursor).Text) then
                        raise Secondary_Configuration with
                          "Missing scrolling text";
                     end if; -- First > Length (Element (Item_Cursor).Text)
                     if Is_In (Element (Element (Item_Cursor).Text, First),
                               Delimiter_Set)
                     then
                        Text_Display_Start := First + 1; -- Skip the delimiter
                        Display_Start := Secondary_Digits'Last;
                        --  Initially only the right most display used.
                     else
                        raise Secondary_Configuration with
                          "Scrolling text expected ',' and found '" & 
                          Element (Element (Item_Cursor).Text, First) & ''';
                     end if; -- Is_In (Element (Element (Item_Cursor).Text ...
                     Time_Remaining := Item_Duration + 1;
                  elsif Time_Remaining = 1 then
                     if Display_Start = Secondary_Digits'First then
                        --  Wait until first character is in left most display
                        --  before stepping to the next character.
                        if Text_Display_Start <
                          Length (Element (Item_Cursor).Text)
                        then
                           Time_Remaining := Item_Duration + 1;
                           Text_Display_Start := @ + 1;
                        end if; -- Text_Display_Start < ...
                     else
                        Time_Remaining := Item_Duration + 1;
                     end if; -- Display_Start = Secondary_Digits'First
                     if Display_Start /= Secondary_Digits'First then
                        Display_Start := Secondary_Digits'Pred (Display_Start);
                     end if; -- Display_Start /= Secondary_Digits'First
                     --  First character moves across display until it eaches
                     --  the left display
                  end if; -- First_Time
                  Update_Scrolling_Text (Display_Brightness,
                                         Element (Item_Cursor).Text, First,
                                         Text_Display_Start, Display_Start);
               when Arbitrary =>
                  Update_Arbitrary (Display_Brightness,
                                    Element (Item_Cursor).Text, Start_At,
                                    First, Last);
               when Blank =>
                  Blank;
            end case; -- Display_Item
            First_Run := False;
         else
            Blank;
         end if; -- Item_Cursor /= Item_Lists.No_Element and then ...
      elsif (Step_Display and Exists (File_Name)) and then
        File_Time /= Modification_Time (File_Name) then
         -- Only test that the file has become available once per second, made
         -- one shot by testing file date/time.
         Initialise_Secondary_Display;
      else
         Blank;
      end if; -- Run
   exception
      when Event: Others =>
         Item_list (Item_Cursor).Valid := False;
         Time_Remaining := Duration_Counters'First;
         First_Run := False; -- Necessary to cause stepping to next Item.
         Put_Error ("Error at line :" &
                    Item_list (Item_Cursor).Item_Number'Img &
                    " Character:" & First'Img, Event);
   end Update_Secondary;

end Secondary_Display;
