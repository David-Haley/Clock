-- Package to provide secondary display functionality.
-- It is assumed that Update_Secondary is called once per second with
-- Step_Display True to update the secondary display contents.
-- Author    : David Haley
-- Created   : 17/07/2019
-- Last Edit : 29/08/2026

--  20260829: Configuration file now json rather that CSV.
--  20260605: Reporting of configuration file date and time made consistent with
--  other configuration files.
--  20260603: Merged Topic_Manager.
--  202600530: Reading of MQTT configuration made single shot.
--  20260525: Display of non scrolling text sourced from a MQTT broker added.
--  20260414: improved location of errors when exceptions are raised.
--  20260412: Limiting the number of exceptions raised due to parsing errors.
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

with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with LED_Declarations; use LED_Declarations;
pragma Warnings (Off, "-gnatwu");
with Clock_Driver; use Clock_Driver;
--  Warning raised by use clause, results in mutiple errors if removed.
pragma Warnings (On, "-gnatwu");
with Shared_User_Interface; use Shared_User_Interface;
with User_Interface_Server; use User_Interface_Server;
with Topic_Manager; use Topic_Manager;

package body Secondary_Display is

   use Clock_LEDs;

   subtype Decimal_Digit is Natural range 0 .. 9;

   type Display_Items is (DDMMYY, MMDDYY, YYMMDD, Time_Zone, Static_Text, 
                          Scrolling_Text, MQTT, Arbitrary, Blank);
   subtype Date_Formats is Display_Items range DDMMYY .. YYMMDD;

   subtype Duration_Counters is Natural range 0 .. 3600;
   subtype Item_Durations is Duration_Counters range
     1 .. Duration_Counters'Last;

   package Time_Changes is new Ada.Containers.Ordered_Maps (Time, Time_Offset);
   use Time_Changes;

   type Secondary_Segments is array (Secondary_Digits, Segments) of Boolean;

   type Items (Display_Item : Display_Items) is record
      Item_Number :  Positive := Positive'First;
      Valid : Boolean := True;
      Item_Duration : Item_Durations := 1;
      case Display_Item is
         when Blank | DDMMYY | MMDDYY | YYMMDD =>
            null;
         when Time_Zone =>
            Time_Change : Time_Changes.Map := Time_Changes.Empty_Map;
         when Static_Text | Scrolling_Text =>
            Text : Unbounded_String := Null_Unbounded_String;
         when MQTT =>
            MQTT_Item_Id : Unbounded_String := Null_Unbounded_String;
         when Arbitrary =>
            Secondary_Segment : Secondary_Segments :=
              [others => [others => False]];
      end case; -- Display_Item
   end record; -- Items

   package Item_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Items);
   use Item_Lists;

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
      when Event: others =>
         Put_Error ("Error in Date item " & Format'Img & " - ", Event);
         raise;
   end Update_Date;

   procedure Update_Time (Current_Time : in Time;
                          Display_Brightness : in Greyscales;
                          Time_Change : in Time_Changes.Map) is

      --  Text Start_At, First and Last are package wide variables.

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Offset_from_UTC : Time_Offset;
      Tc : Time_Changes.Cursor := First (Time_Change);
      Defined : Boolean;
      

   begin -- Update_Time
      --  To produce a display the current time must be after the first change --  and before the last change. If this is not the case the item is not
      Defined := Tc /= Time_Changes.No_Element and then
        (First_Key (Time_Change) <= Current_Time and
         Current_Time < Last_Key (Time_Change));
      while (Defined and Tc /= Time_Changes.No_Element) and then
        Current_Time >= Key (Tc)
      loop
         Offset_from_UTC := Element (Tc);
         Next (Tc);
      end loop; -- (Defined and  Tc /= Time_Changes.No_Element) ...
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
      when Event: others =>
         Blank; 
         Put_Error ("Error in Time item", Event);
         raise;
   end Update_Time;

   procedure Update_Static_Text (Display_Brightness : in Greyscales;
                                 Text : in Unbounded_String) is

      First : Positive := 1;
      
      Char_Position : Secondary_Digits := Secondary_Digits'First;

   begin -- Update_Static_Text
      Blank; -- Initialisation and default if an exception is raised.
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
      when Event: others =>
         Put_Error ("Error in Static_Text item", Event);
         raise;
   end Update_Static_Text;

   procedure Update_Scrolling_Text (Display_Brightness : in Greyscales;
                                    Text : in Unbounded_String;
                                    Text_Display_Start : in Positive;
                                    Display_Start : in Secondary_Digits) is
      
      Current : Positive := Text_Display_Start;
      Char_Position : Secondary_Digits := Display_Start;

   begin -- Update_Scrolling_Text
      loop -- Set one character
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
      when Event: others =>
         Put_Error ("Error in Scrolling_Text item", Event);
         raise;
   end Update_Scrolling_Text;

   procedure Update_MQTT (Display_Brightness : in Greyscales;
                          MQTT_Item_Id : in MQTT_Item_Ids) is
      
      Char_Position : Secondary_Digits := Secondary_Digits'First;
      MQTT_Text :Unbounded_String;
      Mc : Positive;

   begin -- Update_MQTT
      Blank; -- Initialisation and default if an exception is raised.
      MQTT_Text :=
        To_Unbounded_String (Topic_Manager.Get_For_Display (MQTT_Item_Id));
      Mc := 1;
      loop -- Set one character
         if Is_Alphanumeric (Element (MQTT_Text, Mc)) or
           Is_Space (Element (MQTT_Text, Mc))
         then
            if Mc < Length (MQTT_Text) and then
              Element (MQTT_Text, Mc + 1) = '.'
            then
               Set_Character (Char_Position, Element (MQTT_Text, Mc),
                              Display_Brightness, True);
               Mc := @ + 2;
            else
               Set_Character (Char_Position, Element (MQTT_Text, Mc),
                              Display_Brightness);
               Mc := @ + 1;
            end if; -- Mc < Length (MQTT_Text) and then ...Char_Position
         else
            Set_Character (Char_Position, Element (MQTT_Text, Mc),
                           Display_Brightness);
            Mc := @ + 1;
         end if; -- Is_Alphanumeric (Element (MQTT_Text, Mc)) or ...
         exit when Char_Position = Secondary_Digits'Last or
           Mc > Length (MQTT_Text);
         Char_Position := Display_Digits'Succ (Char_Position);
      end loop; -- Set one character
   exception
      when Event: others =>
         Put_Error ("Error in MQTT item", Event);
         raise;
   end  Update_MQTT;

   procedure Update_Arbitrary (Display_Brightness : in Greyscales;
                               Secondary_Segment : in Secondary_Segments) is

   begin -- Update_Arbitrary
      Blank;
      for D in Secondary_Digits loop
         for S in Segments loop
            if Secondary_Segment (D, S) then
               Set_Greyscale (Display_Array (D).Driver,
                              Display_Array (D).
                              Segment_Array (S),
                              Display_Brightness);
            end if; -- Secondary_Segment (D, S)
         end loop; -- S in Segments
      end loop; -- D in Secondary_Digits
   exception
      when Event: others =>
         Put_Error ("Error in Arbitrary item", Event);
         raise;
   end Update_Arbitrary;

   -- State variables of Update_Secondary and Resync_Secondary.
   File_Name : constant String := "Secondary.json";
   File_Time : Time;
   Item_List : Item_Lists.List := Empty_List;
   Item_Cursor : Item_Lists.Cursor;
   Time_Remaining : Duration_Counters := Duration_Counters'First;
   First_Run, First_Time : Boolean := True;
   Run : Boolean := False;
   Text_Display_Start : Positive;
   --  First character of scrolling text to be displayed.
   Display_Start : Secondary_Digits;
   --  Display where the first character of scrolling text is placed.
   Subscribed : Boolean := False;

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

      function Mixed_Digit (Digit : Secondary_Digits) return String is

         --  Returns a mixed case version of the enumerated type
         --  Secondary_Sigits, effectively similar caseing the constant
         --  declarationd.

         Result : String := Digit'Img; -- Assumed all upper case

      begin -- Mixed_Digit
         for I in Positive range 2 .. Result'Length loop
            if Result (I - 1) /= '_' then
               Result (I) := To_Lower (Result (I));
            end if; -- Result (I - 1) /= '_'
         end loop; -- I in Positive range 2 .. Result'Length
         return Result;
      end Mixed_Digit;

      Item_Array_String : constant String := "Item_Array";
      Item_Type_String : constant String := "Item_Type";
      Duration_String : constant String := "Duration";
      Change_String : constant String := "Change";
      Start_String : constant String := "UTC_Start";
      Offset_String : constant String := "UTC_Offset";
      Text_String : constant String := "Text";
      Item_Id_String : constant String := "Item_Id";
      Digit_String : constant String := "Digit_List";

      Parsed : Read_Result;
      Item_Array : JSON_Array := Empty_Array;
      Item_Type : Display_Items;

   begin -- Initialise_Secondary_Display
      Parsed := Read_File (File_Name);
      if Parsed.Success then
         Item_Array := Get (Parsed.Value, Item_Array_String);
      else
         raise Secondary_Configuration with "Parsing error, Line:" &
           Parsed.Error.Line'Img & " Column:" &
           Parsed.Error.Column'Img & " Message : " &
           Format_Parsing_Error(Parsed.Error);
      end if; -- Parsed.Success
      if not Is_Empty (Item_Array) then
         Clear (Item_List);
         for I in Natural range 1 .. Length (Item_Array) loop
            Item_Type := Display_Items'Value (Get (Get (Item_Array, I),
                                                 Item_Type_String));
            declare -- Item declaration block
               Item : Items (Item_Type);
            begin -- Item declaration block
               Item.Item_Number := I;
               Item.Item_Duration :=
                 Get (Get (Item_Array, I), Duration_String);
               case Item_Type is
               when Blank | DDMMYY | MMDDYY | YYMMDD =>
                  null;
               when Time_Zone =>
                  declare -- Change_Array declaration block
                     Change_Array : constant JSON_Array :=
                       Get (Get (Item_Array, I), Change_String);
                     Start : Time;
                     Offset : Integer;
                  begin -- Change_Array declaration block
                     for C in Natural range 1 .. Length (Change_Array) loop
                        Start :=
                          Value (Get (Get (Change_Array, C), Start_String));
                        Offset := Get (Get (Change_Array,C), Offset_String);
                        Include (Item.Time_Change, Start, Time_Offset (Offset));
                     end loop; -- C in Natural range 1 .. Length (Change_Array)
                  end; -- Change_Array declaration block
               when Static_Text | Scrolling_Text =>
                  Item.Text := Get (Get (Item_Array, I), Text_String);
               when MQTT =>
                  Item.MQTT_Item_Id :=
                    Get (Get (Item_Array, I), Item_Id_String);
               when Arbitrary =>
                  for D in Secondary_Digits loop
                     if Has_Field (Get (Get (Item_Array, I), Digit_String),
                                   Mixed_Digit (D))
                     then
                        declare -- Digit_Array declaration block
                           Segment_Array : constant JSON_Array :=
                             Get (Get (Get (Item_Array, I), Digit_String),
                                  Mixed_Digit (D));
                        begin -- Digit_Array declaration block
                           for S in Natural range 1 .. Length (Segment_Array)
                           loop
                              Item.Secondary_Segment (D,
                                Segments'Value (Get (Get (Segment_Array, S))))
                                := True;
                           end loop; -- S in Natural range 1 ...
                        end; -- Digit_Array declaration block
                     end if; -- Has_Field (Get (Item_Array, I), Mixed_Digit (D))
                  end loop; -- D in Secondary_Digits 
               end case; -- Item_Type
               Append (Item_List, Item);
            end; -- Item declaration block
         end loop; -- I in Natural range 1 .. Length (Item_Array)
         File_Time := Modification_Time (File_Name);
         Put_Event ("Read " & File_Name & " " &
                    Local_Image (Modification_Time (File_Name)));
         if not Subscribed and then Topic_Manager.File_Exists then
            --  Readinng subscriotion information is one shot
            Read_Topics;
            Subscribed := True;
            Put_Event ("Read " & Topic_Management_File & " " &
                    Local_Image (Modification_Time (Topic_Management_File)));
         end if; -- not Subscribed and then Topic_Manager.File_Exists
         Resync_Secondary;
      else
         Blank;
         Report_Current_Item (Head ("Blank - Initialise_Secondary_Display",
                                    UI_Strings'Length));
      end if; -- not Is_Empty (Item_Array)
   exception
      when E : others =>
         Put_Error ("Initialise_Secondary_Displey", E);
   end Initialise_Secondary_Display;

   procedure Update_Secondary (Current_Time : in Time;
                               Display_Brightness : in Greyscales;
                               Step_Display : Boolean := False) is

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
            Report_Current_Item (Head ("Item Number" &
              Element (Item_Cursor).Item_Number'Img & ": " &
              Element (Item_Cursor).Display_Item'Img &
              Element (Item_Cursor).Item_Duration'Img,
                                 UI_Strings'Length, ' '));
            if First_Time then
               Time_Remaining := Element (Item_Cursor).Item_Duration;
            end if; -- First_Time
            case Element (Item_Cursor).Display_Item is
               when DDMMYY | MMDDYY | YYMMDD =>
                  Update_Date (Element (Item_Cursor).Display_Item,
                               Current_Time, Display_Brightness);
               when Time_Zone =>
                  Update_Time (Current_Time, Display_Brightness,
                               Element (Item_Cursor).Time_Change);
               when Static_Text =>
                  Update_Static_Text (Display_Brightness,
                                      Element (Item_Cursor).Text);
               when Scrolling_Text =>
                  --  Initialisation and default if an exception is raised.
                  Blank;
                  if First_Time then
                     Text_Display_Start := 1;
                     Display_Start := Secondary_Digits'Last;
                     if Length (Element (Item_Cursor).Text) = 0 then
                        raise Secondary_Configuration with
                          "Missing scrolling text";
                     end if; --Length (Element (Item_Cursor).Text) = 0
                     Time_Remaining := Element (Item_Cursor).Item_Duration + 1;
                  elsif Time_Remaining = 1 then
                     if Display_Start = Secondary_Digits'First then
                        --  Wait until first character is in left most display
                        --  before stepping to the next character.
                        if Text_Display_Start <
                          Length (Element (Item_Cursor).Text)
                        then
                           Time_Remaining :=
                             Element (Item_Cursor).Item_Duration + 1;
                           Text_Display_Start := @ + 1;
                        end if; -- Text_Display_Start < ...
                     else
                        Time_Remaining :=
                          Element (Item_Cursor).Item_Duration + 1;
                     end if; -- Display_Start = Secondary_Digits'First
                     if Display_Start /= Secondary_Digits'First then
                        Display_Start := Secondary_Digits'Pred (Display_Start);
                     end if; -- Display_Start /= Secondary_Digits'First
                     --  First character moves across display until it eaches
                     --  the left display
                  end if; -- First_Time
                  Update_Scrolling_Text (Display_Brightness,
                                         Element (Item_Cursor).Text,
                                         Text_Display_Start, Display_Start);
               when MQTT =>
                  Update_MQTT (Display_Brightness,
                               To_String (Element (Item_Cursor).MQTT_Item_Id));
               when Arbitrary =>
                  Update_Arbitrary (Display_Brightness,
                                    Element (Item_Cursor).Secondary_Segment);
               when Blank =>
                  Blank;
            end case; -- Element (Item_Cursor).Display_Item
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
      when Event: others =>
         Item_List (Item_Cursor).Valid := False;
         Time_Remaining := Duration_Counters'First;
         First_Run := False; -- Necessary to cause stepping to next Item.
         Put_Error ("Error at line :" &
                    Item_List (Item_Cursor).Item_Number'Img, Event);
         --  The exception is not propagated to allow the clock to continue to
         --  display valid items when one or more invalid items are present.
   end Update_Secondary;

end Secondary_Display;
