-- Provides for chiming on the hour.A sound file is required for each hour the
-- clock is required to chime. The configuration file containing one line for
-- each hour that the clock is required to chime. Up to 24 line are read. If
-- an entry for an hour is missing no chime is played. If the configuration file
-- is absent chiming is disabled.

-- Author    : David Haley
-- Created   : 28/03/2019
-- Last Edit : 09/04/2025

-- 20250409 : Ude of DJH.Parse_CSV and reporting of the Chimes.csv file
-- date/time via Put_Event added. The Volume_Command and Play Command are now
-- read from the general configuration file.
-- 20250406 : Default_Volume now read from General_Configuration.
-- 20230319 : Name of volume control changed from Headphones to PCM, required
-- as a result of Pi OS update.
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20220124 : Pragma Warnings added to suppress warning related to potential
-- blocking in Set_Volume.
-- 20220120 : Raise exception on Volume set failure. Fix Set_Volume and set
-- volume in startup. Test_Volume added.
-- 20220118 : Implemention of protection for Chime data structures
-- 20220116 : Configuration file is reread whenever chiming is enabled.
-- Raise_Volume, Lower_Volume and Get_Volume added
-- 20190730 : Exception handling made consistent with reading of other
-- configuration files. Large jumps in time managed.
-- 20190707 : Spelling of Chiming corrected
-- 20190420 : fully qualified parh to aplay required as argument to spawn.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with User_Interface_Server; use User_Interface_Server;
with General_Configuration; use General_Configuration;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with DJH.Parse_CSV;

package body Chime is

   Configuration_File_Name : constant String := "Chimes.csv";
   Volume_Test_File_Name : constant String := "volume_test.wav";

   subtype Commands is Unbounded_String;

   type Chime_Lists is array (Hour_Number) of Commands;

   Chime_Error : exception;

   package Command_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Commands);

   package Command_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Command_Queue_Interface);
   use Command_Queues;

   Command_Queue : Command_Queues.Queue;

   protected Chime_State is

      procedure Chiming;
      -- Enables chiming

      procedure Silent;
      -- Disables Chiming

      procedure Raise_Volume;
      -- Raises volume 1% for each call

      procedure Lower_Volume;
      -- Lowers volume 1% for each call

      function Get_Chiming return Boolean;
      -- returns true if chiming is enabled

      function Read_Chime_List (This_Hour : in Hour_Number) return Commands;
      -- Returns command to spawn aplayer;

   private

      Previous_File_Time : Time := Value ("2019-01-01 00:00:00");
      -- A time before the hardware existed!
      Chiming_Enabled : Boolean := False;
      Chime_List : Chime_Lists := (others => Null_Unbounded_String);
      Current_Volume : Chime_Volumes := Default_Volume;

   end Chime_State;

   task Strike_Hour is
      entry End_Chiming;
   end Strike_Hour;

   task Run_Commands is
   end Run_Commands;

   procedure Chiming is

      -- Enables chiming

   begin -- Chiming
      Chime_State.Chiming;
   end Chiming;

   procedure Silent is

      -- Disables Chiming

   begin -- Silent
      Chime_State.Silent;
   end Silent;

   procedure Raise_Volume is

      -- Raises volume 1% for each call

   begin -- Raise_Volume
      Chime_State.Raise_Volume;
   end Raise_Volume;

   procedure Lower_Volume is

      -- Lowers volume 1% for each call

   begin -- Lower_Volume
      Chime_State.Lower_Volume;
   end Lower_Volume;

   procedure Test_Volume is

      -- Uses aplay to play volume_test.wav from directory from which the
      -- appliction was started.

   begin -- Test_Volume
      if Exists (Volume_Test_File_Name) then
         Command_Queue.Enqueue (To_Unbounded_String (Play_Command & ' ' &
           Volume_Test_File_Name));
      else
         raise Chime_Error with "File not found: " & Volume_Test_File_Name;
      end if; -- Exists (Volume_Test_File_Name)
   exception
      when Event : others =>
         Put_Error ("Test_Volume: ", Event);
   end Test_Volume;


   procedure End_Chiming is

      -- End Chiming process to allow main application to exit

   begin -- End_Chiming
      Strike_Hour.End_Chiming;
   end End_Chiming;

   protected body Chime_State is

      procedure Read_Chime_List (Chime_File_Name : in String;
                                 Chime_List : out Chime_Lists) is
         -- Reads in the file containing the sound files to be played

         type Header is (Hour, File_Name);

          package Parse_Chime is new DJH.Parse_CSV (Header);
          use Parse_Chime;
         
         This_Hour : Hour_Number;

      begin -- Read_Chime_List
         If Exists (Chime_File_Name) and then
           Modification_Time (Chime_File_Name) /= Previous_File_Time then
           -- Only reread file if it has been changed.
            Chime_List := (others => Null_Unbounded_String);
            Read_Header (Chime_File_Name);
            while Next_Row loop
               This_Hour := Hour_Number'Value (Get_Value (Hour));
               if Exists (Get_Value (File_Name)) then
                  Chime_List (This_Hour) :=
                    To_Unbounded_String (Play_Command & ' ' &
                                         Get_Value (File_Name));
               else
                  raise Chime_Error with
                    "File not found: " & Get_Value (File_Name) & " at row" &
                    Row_Number'Img;
               end if; --  Exists (Get_Value (File_Name))
            end loop; --  Next_Row
            Put_Event ("Read " & Chime_File_Name & ' ' &
              Local_Image (Modification_Time (Chime_File_Name)));
              Previous_File_Time := Modification_Time (Chime_File_Name);
            Close_CSV;
         end if; -- Exists (Chime_File_Name) and then ...
      exception
         when Event : others =>
            Put_Error ("Read_Chime_List: ", Event);
      end Read_Chime_List;

      procedure Set_Volume is

         -- Calls amixer to set the OS volume.

      begin -- Set_Volume
         pragma Warnings (Off);
         Command_Queue.Enqueue (To_Unbounded_String (Volume_Command & ' ' &
           Current_Volume'Img & '%'));
         pragma Warnings (On);
      exception
         when Event : others =>
            Put_Error ("Set_Volume: ", Event);
      end Set_Volume;

      procedure Chiming is

         -- Enables chiming

      begin -- Chiming
         if not Chime_State.Chiming_Enabled then
            -- Only reread configuration when chiming first enabled because
            -- reading the external files is potentially time consuming and
            -- could possibly cause clock to not update display on time!
            if Exists (Configuration_File_Name) then
               Read_Chime_List (Configuration_File_Name, Chime_List);
               Set_Volume;
               Chiming_Enabled := True;
            else
               Chiming_Enabled := False;
            end if; -- Exists (Configuration_File_Name)
         end if; -- not Chime_State.Chiming_Enabled
         Report_Chiming (Chiming_Enabled, Current_Volume);
      end Chiming;

      procedure Silent is

         -- Disables Chiming

      begin -- Silent
         Chiming_Enabled := False;
         Report_Chiming (Chiming_Enabled, Current_Volume);
      end Silent;

      procedure Raise_Volume is

         -- Raises volume 1% for each call

      begin -- Raise_Volume
         if Current_Volume < Chime_Volumes'Last then
            Current_Volume := Current_Volume + 1;
            Set_Volume;
         end if; -- Current_Volume < Chime_Volumes'Last
         Report_Chiming (Chiming_Enabled, Current_Volume);
      end Raise_Volume;

      procedure Lower_Volume is

         -- Lowers volume 1% for each call

      begin -- Lower_Volume
         if Current_Volume > Chime_Volumes'First then
            Current_Volume := Current_Volume - 1;
            Set_Volume;
         end if; -- Current_Volume > Chime_Volumes'First
         Report_Chiming (Chiming_Enabled, Current_Volume);
      end Lower_Volume;

      function Get_Chiming return Boolean is

         -- returns true if chiming is enabled

      begin -- Get_Chiming
         return Chiming_Enabled;
      end Get_Chiming;

      function Read_Chime_List (This_Hour : in Hour_Number) return
        Commands is

         -- Returns command to spawn aplayer;

      begin -- Read_Chime_List
         return Chime_List (This_Hour);
      end Read_Chime_List;

   end Chime_State;

   task body Strike_Hour is

      function Next_Hour return Time is

         -- This function returns the time of the the next hour that is zero
         -- minutes and seconds.

         One_Hour : constant Duration := 3600.0;

         Now : Time := Clock;
         Year : Year_Number;
         Month : Month_Number;
         Day : Day_Number;
         Hour : Hour_Number;
         Minute : Minute_Number;
         Second : Second_Number;
         Sub_Second : Second_Duration;
         Leap_Second : Boolean;

      begin -- Next_Hour
         Split (Now, Year, Month, Day,
                Hour, Minute, Second, Sub_Second, Leap_Second,
                UTC_Time_Offset (Now));
         return Time_Of (Year, Month, Day, Hour, 0, 0, 0.0, Leap_Second,
                         UTC_Time_Offset (Now))
           + One_Hour; -- next hour exactly as Time
      end Next_Hour;

      Time_Step : Duration := 120.0;
      -- Maximum step in time due to power failure or otherwise before chiming
      -- is suppressed.
      Next_Time : Time;
      This_Hour : Hour_Number;
      Run_Chime : Boolean := True;
      Command : Commands;

   begin -- Strike_Hour
      Next_Time := Next_Hour;
      while Run_Chime loop
         select
            accept End_Chiming  do
               Run_Chime := False;
               Command_Queue.Enqueue (Null_Unbounded_String);
               -- Terminate Run_Commands
            end End_Chiming;
         or
            delay until Next_Time;
            This_Hour := Hour (Next_Time, UTC_Time_Offset (Next_Time));
            Command := Chime_State.Read_Chime_List (This_Hour);
            if Command /= Null_Unbounded_String and
              Chime_State.Get_Chiming and
              -- Chiming permmited and sound file defined for this hour
              Clock <= Next_Time + Time_Step and
              Clock >= Next_Time - Time_Step then
               -- There has not been a big step in time. Prevents multiple
               -- chimes being played if there is a big step in time, for
               -- example a restart after a power failure lasting several
               -- hours.
               Command_Queue.Enqueue (Command);
            end if; -- Command /= Null_Unbounded_String and ...
            Next_Time := Next_Hour;
         end select;
      end loop; -- Run_Chime
   end Strike_Hour;

   task body Run_Commands is

      Arguments : Argument_List_Access := null;
      Exit_Status : Integer;
      Command : Commands;

   begin -- Run_Commands
      loop -- spawn one command
         Command_Queue.Dequeue (Command);
         exit when Command = Null_Unbounded_String;
         Arguments := Argument_String_To_List (To_String (Command));
         Exit_Status := Spawn
           (Program_Name => Arguments (Arguments'First).all,
            Args => Arguments (Arguments'First + 1 .. Arguments'Last));
         Free (Arguments);
         if Exit_Status /= 0 then
            raise Chime_Error with "Exit_Statue;" & Exit_Status'Img &
              " Command: """ & To_String (Command) & """";
         end if; -- Exit_Status /= 0
      end loop; -- spawn one command
   exception
      when Event : others =>
         Put_Error ("Run_Commands", Event);
   end Run_Commands;

end Chime;
