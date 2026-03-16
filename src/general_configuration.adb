-- This package reads the general clock confifuration;

-- Author    : David Haley
-- Created   : 05/04/2025
-- Last Edit : 17/03/2026

-- 20260317: MQTT broker config, Scroll_Delay_Ms and Force_Uppercase_Default
-- added as optional configuration keys (gracefully absent from older CSV).
-- 20250510: Providing for simulated sweep hand modes to be set on startup.
-- 20250411 : Corrected Minimum_Chime return value
-- 20250410 : Read_General_Configuration, Play_Command and Volume_Command added.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with DJH.Parse_Csv;
with MQTT;

package body General_Configuration is

   protected Configuration is
   
      procedure Read;
      -- Reads configuration from General_Configuration.csv.

      entry Minimum_Brightness (L : out Lit_Greyscales);
      -- Returns the minimum dislpay brightness

      entry Minimum_Chime (G : out Greyscales);
      -- Returns mimimum brightness level at which chiming should occur.

      entry Sweep_Mode (S : out Sweep_Modes);
      -- Returns the startup mode ot the simulated sweep hand.
      
      entry Cycle_Sweep_Mode;
      -- Cycles through sweep modes one step per call.

      entry Gamma (G : out Gammas);
      -- Returns the gamma value to be applied to the leading edge LED of the
      -- simulated sweep hand, only applies to Smooth and With_Tail modes.

      entry Default_Volume (C : out Chime_Volumes);
      -- The initial chime volume of the clock when started.

      entry Play_Command (Pc : out Unbounded_String);
      -- Returns the command line string used to invoke the sound player
      -- application.

      entry Volume_Command (Vc : out Unbounded_String);
      -- Returns the command line string used to invoke the volume control
      -- appliation.

      entry Scroll_Delay_Ms (Ms : out Natural) when Defined;
      -- Milliseconds between each scroll step.

      entry Force_Uppercase_Default (F : out Boolean) when Defined;
      -- When True, lowercase letters map to uppercase glyphs.

      entry MQTT_Broker_Host (H : out Unbounded_String) when Defined;
      -- Hostname or IP of the MQTT broker.

      entry MQTT_Broker_Port (P : out Natural) when Defined;
      -- TCP port of the MQTT broker.

      entry MQTT_User_Name (U : out Unbounded_String) when Defined;
      -- MQTT broker username.

      entry MQTT_Password (Pw : out Unbounded_String) when Defined;
      -- MQTT broker password.

      entry MQTT_QoS (Q : out Natural) when Defined;
      -- MQTT Quality of Service level (0, 1 or 2).

      entry MQTT_Keep_Alive (Ka : out Positive) when Defined;
      -- MQTT keep-alive interval in seconds.

   private

      Defined : Boolean := False;
      Prot_Minimum_Brightness : Lit_Greyscales := 1;
      Prot_Minimum_Chime : Greyscales := Greyscales'Last; -- disabled
      Prot_Sweep_Mode : Sweep_Modes := Normal;
      Prot_Gamma : Gammas := (Gammas'First + Gammas'Last) / 2.0;
      Prot_Default_Volume : Chime_Volumes := Chime_Volumes'First; -- silent
      Prot_Play_Command, Prot_Volume_Command : Unbounded_String :=
        Null_Unbounded_String;
      Prot_Scroll_Delay_Ms : Natural := 500;
      Prot_Force_Uppercase_Default : Boolean := True;
      Prot_MQTT_Broker_Host : Unbounded_String := Null_Unbounded_String;
      Prot_MQTT_Broker_Port : Natural := 1883;
      Prot_MQTT_User_Name : Unbounded_String := Null_Unbounded_String;
      Prot_MQTT_Password : Unbounded_String := Null_Unbounded_String;
      Prot_MQTT_QoS : Natural := 0;
      Prot_MQTT_Keep_Alive : Positive := 60;

   end Configuration;

   protected body Configuration is

      procedure Read is
         -- Reads configuration from General_Configuration.csv.

         File_Name : constant String := "General_Configuration.csv";

         type Header is (Minimum_Brightness, Minimum_Chime, Sweep_Mode, Gamma,
                         Default_Volume, Play_Command, Volume_Command,
                         Scroll_Delay_Ms, Force_Uppercase_Default,
                         MQTT_Broker_Host, MQTT_Broker_Port, MQTT_User_Name,
                         MQTT_Password, MQTT_QoS, MQTT_Keep_Alive);

         package Parse_Configuration is new DJH.Parse_CSV (Header);
         use Parse_Configuration;

         -- Helper to safely read optional integer values
         function Safe_Integer (S : String; Default : Natural) return Natural is
         begin
            return Natural'Value (S);
         exception
            when others => return Default;
         end Safe_Integer;

         -- Helper to safely read optional boolean values
         function Safe_Boolean (S : String; Default : Boolean) return Boolean is
         begin
            return Boolean'Value (S);
         exception
            when others => return Default;
         end Safe_Boolean;

      begin -- Read
         Read_Header (File_Name);
         if Next_Row then
            Prot_Minimum_Brightness :=
              Lit_Greyscales'Value (Get_Value (Minimum_Brightness));
            Prot_Minimum_Chime :=
              Greyscales'Value (Get_Value (Minimum_Chime));
            Prot_Sweep_Mode := Sweep_Modes'Value (Get_Value (Sweep_Mode));
            Prot_Gamma := Gammas'Value (Get_Value (Gamma));
            Prot_Default_Volume :=
              Chime_Volumes'Value (Get_Value (Default_Volume));
            Prot_Play_Command := To_Unbounded_String (Get_Value (Play_Command));
            Prot_Volume_Command :=
              To_Unbounded_String (Get_Value (Volume_Command));
            -- Optional new keys (gracefully default if CSV is old)
            Prot_Scroll_Delay_Ms :=
              Safe_Integer (Get_Value (Scroll_Delay_Ms), 500);
            Prot_Force_Uppercase_Default :=
              Safe_Boolean (Get_Value (Force_Uppercase_Default), True);
            Prot_MQTT_Broker_Host :=
              To_Unbounded_String (Get_Value (MQTT_Broker_Host));
            Prot_MQTT_Broker_Port :=
              Safe_Integer (Get_Value (MQTT_Broker_Port), 1883);
            Prot_MQTT_User_Name :=
              To_Unbounded_String (Get_Value (MQTT_User_Name));
            Prot_MQTT_Password :=
              To_Unbounded_String (Get_Value (MQTT_Password));
            Prot_MQTT_QoS :=
              Safe_Integer (Get_Value (MQTT_QoS), 0);
            Prot_MQTT_Keep_Alive :=
              Safe_Integer (Get_Value (MQTT_Keep_Alive), 60);
            if Prot_MQTT_Keep_Alive < 1 then
               Prot_MQTT_Keep_Alive := 60;
            end if;
            Defined := True;
         else
            raise CSV_Error with "Data row missing";
         end if; -- Next_Row;
         Put_Event ("Read " & File_Name & ' ' &
           Local_Image (Modification_Time (File_Name)));
         Close_CSV;
      exception
         when E : others =>
            Put_Error ("General configuration - ", E);
      end Read;

      entry Minimum_Brightness (L : out Lit_Greyscales) when Defined is
         -- Returns the minimum dislpay brightness

      begin -- Minimum_Brighness
         L := Prot_Minimum_Brightness;
      end Minimum_Brightness;

      entry Minimum_Chime (G : out Greyscales) when Defined is
         -- Returns mimimum brightness level at which chiming should occur.

      begin -- Minimum_Chime
         G := Prot_Minimum_Chime;
      end Minimum_Chime;

      entry Sweep_Mode (S : out Sweep_Modes) when Defined is
         -- Returns the startup mode ot the simulated sweep hand.
         
      begin -- Sweep_Mode
         S := Prot_Sweep_Mode;
      end Sweep_Mode;
      
      entry Cycle_Sweep_Mode when Defined is
         -- Cycles through sweep modes one step per call.
         
      begin --
         if Prot_Sweep_Mode < Sweep_Modes'Last then
            Prot_Sweep_Mode := Sweep_Modes'Succ (Prot_Sweep_Mode);
         else
            Prot_Sweep_Mode := Sweep_Modes'First;
         end if; -- Prot_Sweep_Mode < Sweep_Modes'Last
      end Cycle_Sweep_Mode;

      entry Gamma (G : out Gammas) when Defined is
         -- Returns the gamma value to be applied to the leading edge LED of the
         -- simulated sweep hand, only applies to Smooth and With_Tail modes.

      begin -- Gamma
         G := Prot_Gamma;
      end Gamma;

      entry Default_Volume (C : out Chime_Volumes) when Defined is
         -- The initial chime volume of the clock when started.

      begin -- Default_Volume
         C := Prot_Default_Volume;
      end Default_Volume;

      entry Play_Command (Pc : out Unbounded_String) when Defined is
         -- Returns the command line string used to invoke the sound player
         -- application.
   
      begin -- Play_Command
         Pc := Prot_Play_Command;
      end Play_Command;
   
      entry Volume_Command (Vc : out Unbounded_String) when Defined is
         -- Returns the command line string used to invoke the volume control
         -- appliation.

      begin -- Volume_Command
         Vc := Prot_Volume_Command;
      end Volume_Command;

      entry Scroll_Delay_Ms (Ms : out Natural) when Defined is
         -- Milliseconds between each scroll step.

      begin -- Scroll_Delay_Ms
         Ms := Prot_Scroll_Delay_Ms;
      end Scroll_Delay_Ms;

      entry Force_Uppercase_Default (F : out Boolean) when Defined is
         -- When True, lowercase letters map to uppercase glyphs.

      begin -- Force_Uppercase_Default
         F := Prot_Force_Uppercase_Default;
      end Force_Uppercase_Default;

      entry MQTT_Broker_Host (H : out Unbounded_String) when Defined is
         -- Hostname or IP of the MQTT broker.

      begin -- MQTT_Broker_Host
         H := Prot_MQTT_Broker_Host;
      end MQTT_Broker_Host;

      entry MQTT_Broker_Port (P : out Natural) when Defined is
         -- TCP port of the MQTT broker.

      begin -- MQTT_Broker_Port
         P := Prot_MQTT_Broker_Port;
      end MQTT_Broker_Port;

      entry MQTT_User_Name (U : out Unbounded_String) when Defined is
         -- MQTT broker username.

      begin -- MQTT_User_Name
         U := Prot_MQTT_User_Name;
      end MQTT_User_Name;

      entry MQTT_Password (Pw : out Unbounded_String) when Defined is
         -- MQTT broker password.

      begin -- MQTT_Password
         Pw := Prot_MQTT_Password;
      end MQTT_Password;

      entry MQTT_QoS (Q : out Natural) when Defined is
         -- MQTT Quality of Service level (0, 1 or 2).

      begin -- MQTT_QoS
         Q := Prot_MQTT_QoS;
      end MQTT_QoS;

      entry MQTT_Keep_Alive (Ka : out Positive) when Defined is
         -- MQTT keep-alive interval in seconds.

      begin -- MQTT_Keep_Alive
         Ka := Prot_MQTT_Keep_Alive;
      end MQTT_Keep_Alive;

   end Configuration;

   function Minimum_Brightness return Lit_Greyscales is
      -- Returns the minimum dislpay brightness

      Result :  Lit_Greyscales;

   begin -- Minimum_Brightness
      Configuration.Minimum_Brightness (Result);
      return Result;
   end Minimum_Brightness;

   function Minimum_Chime return Greyscales is
      -- Returns mimimum brightness level at which chiming should occur.

      Result :  Greyscales;

   begin -- Minimum_Chime
      Configuration.Minimum_Chime (Result);
      return Result;
   end Minimum_Chime;

   function Sweep_Mode return Sweep_Modes is
      -- Returns the startup mode ot the simulated sweep hand.
      
      Result : Sweep_Modes;
      
   begin -- Sweep_Mode
      Configuration.Sweep_Mode (Result);
      return Result;
   end Sweep_Mode;
   
   procedure Cycle_Sweep_Mode is
      -- Cycles through sweep modes, stepping to a new mode each time it is
      -- called.
      
   begin -- Cycle_Sweep_Mode
      Configuration.Cycle_Sweep_Mode;
   end Cycle_Sweep_Mode;

   function Gamma return Gammas is
      -- Returns the gamma value to be applied to the leading edge LED of the
      -- simulated sweep hand, only applies to Smooth and With_Tail modes.

      Result : Gammas;

   begin -- Gamma
      Configuration.Gamma (Result);
      return Result;
   end Gamma;

   function Default_Volume return Chime_Volumes is
      -- The initial chime volume of the clock when started.

      Result : Chime_Volumes;

   begin -- Default_Volume
      Configuration.Default_Volume (Result);
      return Result;
   end Default_Volume;

   function Play_Command return String is
   -- Returns the command line string used to invoke the sound player
   -- application.

      Result : Unbounded_String;
   
      begin -- Play_Command
         Configuration.Play_Command (Result);
         return To_String (Result);
      end Play_Command;
   
   function Volume_Command return String is
   -- Returns the command line string used to invoke the volume control
   -- appliation.

      Result : Unbounded_String;

      begin -- Volume_Command
         Configuration.Volume_Command (Result);
         return To_String (Result);
      end Volume_Command;

   function Scroll_Delay_Ms return Natural is
   -- Milliseconds between each scroll step.

      Result : Natural;

      begin -- Scroll_Delay_Ms
         Configuration.Scroll_Delay_Ms (Result);
         return Result;
      end Scroll_Delay_Ms;

   function Force_Uppercase_Default return Boolean is
   -- When True, lowercase letters map to uppercase glyphs.

      Result : Boolean;

      begin -- Force_Uppercase_Default
         Configuration.Force_Uppercase_Default (Result);
         return Result;
      end Force_Uppercase_Default;

   function MQTT_Broker_Host return String is
   -- Hostname or IP of the MQTT broker.

      Result : Unbounded_String;

      begin -- MQTT_Broker_Host
         Configuration.MQTT_Broker_Host (Result);
         return To_String (Result);
      end MQTT_Broker_Host;

   function MQTT_Broker_Port return Natural is
   -- TCP port of the MQTT broker.

      Result : Natural;

      begin -- MQTT_Broker_Port
         Configuration.MQTT_Broker_Port (Result);
         return Result;
      end MQTT_Broker_Port;

   function MQTT_User_Name return String is
   -- MQTT broker username.

      Result : Unbounded_String;

      begin -- MQTT_User_Name
         Configuration.MQTT_User_Name (Result);
         return To_String (Result);
      end MQTT_User_Name;

   function MQTT_Password return String is
   -- MQTT broker password.

      Result : Unbounded_String;

      begin -- MQTT_Password
         Configuration.MQTT_Password (Result);
         return To_String (Result);
      end MQTT_Password;

   function MQTT_QoS return Natural is
   -- MQTT Quality of Service level (0, 1 or 2).

      Result : Natural;

      begin -- MQTT_QoS
         Configuration.MQTT_QoS (Result);
         return Result;
      end MQTT_QoS;

   function MQTT_Keep_Alive return Positive is
   -- MQTT keep-alive interval in seconds.

      Result : Positive;

      begin -- MQTT_Keep_Alive
         Configuration.MQTT_Keep_Alive (Result);
         return Result;
      end MQTT_Keep_Alive;

begin -- General_Configuration
   Configuration.Read;
end General_Configuration;
