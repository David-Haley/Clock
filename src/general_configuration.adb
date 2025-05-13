-- This package reads the general clock confifuration;

-- Author    : David Haley
-- Created   : 05/04/2025
-- Last Edit : 10/05/2025

-- 20250510: Providing for simulated sweep hand modes to be set on startup.
-- 20250411 : Corrected Minimum_Chime return value
-- 20250410 : Read_General_Configuration, Play_Command and Volume_Command added.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with DJH.Parse_Csv;

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

   private

      Defined : Boolean := False;
      Prot_Minimum_Brightness : Lit_Greyscales := 1;
      Prot_Minimum_Chime : Greyscales := Greyscales'Last; -- disabled
      Prot_Sweep_Mode : Sweep_Modes := Normal;
      Prot_Gamma : Gammas := (Gammas'First + Gammas'Last) / 2.0;
      Prot_Default_Volume : Chime_Volumes := Chime_Volumes'First; -- silent
      Prot_Play_Command, Prot_Volume_Command : Unbounded_String :=
        Null_Unbounded_String;

   end Configuration;

   protected body Configuration is

      procedure Read is
         -- Reads configuration from General_Configuration.csv.

         File_Name : constant String := "General_Configuration.csv";

         type Header is (Minimum_Brightness, Minimum_Chime, Sweep_Mode, Gamma,
                         Default_Volume, Play_Command, Volume_Command);

         package Parse_Configuration is new DJH.Parse_CSV (Header);
         use Parse_Configuration;

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
      
begin -- General_Configuration
   Configuration.Read;
end General_Configuration;
