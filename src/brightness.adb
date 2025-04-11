-- This package manages the Clock Brightness configuration file.

-- Author    : David Haley
-- Created   : 02/07/2019
-- Last Edit : 09/04/2025

-- 20250409 : Reporting of brightness file modification time.
-- 20250405 : Minimum_Brightness, Chime_Brightness and Gamma removed to gereral
-- configuration. DJH.Parse_CSV used to read in corrections. Brightness Records
-- Removed.
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20190722 : Reading and writing of Gamma implemented.
-- 20190717 : Lit_Greyscales corrected
-- 20190715 : Exception handling, corrected delimiter test. Open replaced with
-- Create in Write_Brightness.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with DJH.Parse_CSV;

package body Brightness is

   Default_Correction : constant Corrections := 31;
   File_Name : constant string := "Brightness";
   Current_Extension : constant string := "csv";
   Backup_Extension : constant string := "bak";
   type Brightness_Header is
     (LED_Driver_Name, Output_Number, Dot_Correction_Value);

   function Read_Brightness_Config return Dot_Corrections is
      -- Reads dot corrections from CSV file.

      package Parse_Brightness is new DJH.Parse_CSV (Brightness_Header);
      use Parse_Brightness;

      Dot_Correction : Dot_Corrections :=
        (others => (others => Default_Correction));
      LED_Driver : LED_Drivers;
      LED_Channel : LED_Channels;

   begin -- Read_Brightness_Config
      begin -- Read header exception block
         if Exists (Compose (Name => File_Name,
                             Extension => Current_Extension)) then
            Read_Header (Compose (Name => File_Name,
                                  Extension => Current_Extension));
            Put_Event ("Read " & Compose (Name => File_Name,
              Extension => Current_Extension) & ' ' &
              Local_Image (Modification_Time (Compose (Name => File_Name,
              Extension => Current_Extension))));
         else
            Put_Event ("Brightness file " &
                         Compose (Name => File_Name,
                                  Extension => Current_Extension) &
                         " not found, attempting to read backup file.");
            if Exists (Compose (Name => File_Name,
                                Extension => Backup_Extension)) then
               Read_Header (Compose (Name => File_Name,
                                     Extension => Backup_Extension));
               Put_Event ("Read " & Compose (Name => File_Name,
                 Extension => Backup_Extension) & ' ' &
                 Local_Image (Modification_Time (Compose (Name => File_Name,
                 Extension => Backup_Extension))));
            else
               raise CSV_Error with "Brightness file not found";
            end if; -- Exists (Compose (Name => File_Name ...
         end if; -- Exists (Compose (Name => File_Name ...
      exception
         when Event: others =>
            Put_Error ("Unable to read brightness file header", Event);
            raise;
      end; -- Read header exception block
      begin -- Read brightness values exception block
         while Next_Row loop
            -- read one correction
            LED_Driver := LED_Drivers'Value (Get_Value (LED_Driver_Name));
            LED_Channel := LED_Channels'Value (Get_Value (Output_Number));
            Dot_Correction (LED_Driver, LED_Channel) :=
              Corrections'Value (Get_Value (Dot_Correction_Value));
         end loop; -- Next_Row
      exception
         when Event: others =>
            Put_Error ("Bad brightness data at :" & Row_Number'Img, Event);
            raise;
      end; -- Read brightness values exception block
      Close_CSV;
      return Dot_Correction;
   end Read_Brightness_Config;

   procedure Write_Brightness_Config (Dot_Correction : in Dot_Corrections) is
      -- Writes dot correction file.

      Temporary_Extension : constant string := "tmp";
      Delimiter : constant character := ',';

      Text_File : File_Type;

   begin -- Write_Brightness_Config
      Create (Text_File, Out_File,
              Compose (Name => File_Name, Extension => Temporary_Extension));
      for H in Brightness_Header loop
         Put (Text_File, H'Img);
         if H = Brightness_Header'Last then
            New_Line (Text_File);
         else
            Put (Text_File, Delimiter);
         end if; -- H = Brightness_Header'Las
      end loop; -- H in Brightness_Header
      for LED_Driver in LED_Drivers loop
         for LED_Channel in LED_Channels loop
            Put_Line (Text_File, LED_Driver'img & Delimiter &
                        LED_Channel'Img & Delimiter &
                        Dot_Correction (LED_Driver, LED_Channel)'Img);
         end loop; --  LED_Channel in LED_Channels
      end loop; -- LED_Driver in LED_Drivers
      Close (Text_File);
      if Exists (Compose (Name => File_Name,
                          Extension => Backup_Extension)) then
         Delete_File (Compose (Name => File_Name,
                          Extension => Backup_Extension));
      end if; -- Exists (Compose (Name => File_Name, ...
      if Exists (Compose (Name => File_Name,
                          Extension => Current_Extension)) then
         Rename (Compose (Name => File_Name, Extension => Current_Extension),
                 Compose (Name => File_Name, Extension => Backup_Extension));
      end if; -- Exists (File_Name & Current_Extension)
      Rename (Compose (Name => File_Name, Extension => Temporary_Extension),
              Compose (Name => File_Name, Extension => Current_Extension));
   end Write_Brightness_Config;

end Brightness;
