-- Author    : David Haley
-- Created   : 05/04/2025
-- Last Edit : 06/04/202
-- This package reads the general clock confifuration;

with Ada.Directories; use Ada.Directories;
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

      entry Gamma (G : out Gammas);
      -- Returns the gamma value to be applied to the leading edge LED of the
      -- simulated sweep hand.

      entry Default_Volume (C : out Chime_Volumes);
      -- The initial chime volume of the clock when started.

   private

      Defined : Boolean := False;
      Prot_Minimum_Brightness : Lit_Greyscales := 1;
      Prot_Minimum_Chime : Greyscales := Greyscales'Last; -- disabled
      Prot_Gamma : Gammas := (Gammas'First + Gammas'Last) / 2.0;
      Prot_Default_Volume : Chime_Volumes := Chime_Volumes'First; -- silent

   end Configuration;

   protected body Configuration is

      procedure Read is
         -- Reads configuration from General_Configuration.csv.

         File_Name : constant String := "General_Configuration.csv";

         type Header is (Minimum_Brightness, Minimum_Chime, Gamma,
                         Default_Volume);

         package Parse_Configuration is new DJH.Parse_CSV (Header);
         use Parse_Configuration;

      begin -- Read
         Read_Header (File_Name);
         if Next_Row then
            Prot_Minimum_Brightness :=
              Lit_Greyscales'Value (Get_Value (Minimum_Brightness));
            Prot_Minimum_Chime :=
              Greyscales'Value (Get_Value (Minimum_Chime));
            Prot_Gamma := Gammas'Value (Get_Value (Gamma));
            Prot_Default_Volume :=
              Chime_Volumes'Value (Get_Value (Default_Volume));
            Defined := True;
         else
            raise CSV_Error with "Data row missing";
         end if; -- Next_Row;
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
         G := Prot_Minimum_Brightness;
      end Minimum_Chime;

      entry Gamma (G : out Gammas) when Defined is
         -- Returns the gamma value to be applied to the leading edge LED of the
         -- simulated sweep hand.

      begin -- Gamma
         G := Prot_Gamma;
      end Gamma;

      entry Default_Volume (C : out Chime_Volumes) when Defined is
         -- The initial chime volume of the clock when started.

      begin -- Default_Volume
         C := Prot_Default_Volume;
      end Default_Volume;

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

   function Gamma return Gammas is
      -- Returns the gamma value to be applied to the leading edge LED of the
      -- simulated sweep hand.

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

begin -- General_Configuration
   Configuration.Read;
end General_Configuration;
