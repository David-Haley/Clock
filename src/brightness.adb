-- This package manages the Clock Brightness configuration file.
-- Author    : David Haley
-- Created   : 02/07/2019
-- Last Edit : 20/08/2022
-- 20220820 : Events_and_Errors moved to DJH.Events_and_Errors.
-- 20190722 : Reading and writing of Gamma implemented.
-- 20190717 : Lit_Greyscales corrected
-- 20190715 : Exception handling, corrected delimiter test. Open replaced with
-- Create in Write_Brightness.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;

package body Brightness is

   File_Name : constant string := "Brightness";
   Temporary_Extension : constant string := ".tmp";
   Current_Extension : constant string := ".csv";
   Backup_Extension : constant string := ".bak";
   Delimiter : constant character := ',';
   LED_Driver : LED_Drivers;
   LED_Channel : LED_Channels;

   package Lit_Greyscale_IO is new Ada.Text_IO.Modular_IO (Lit_Greyscales);

   package Gamma_IO is new Ada.Text_IO.Float_IO (Gammas);

   function Read_Brightness_Config return Brighness_Records is

      Brightness_Error : exception;

      Result : Brighness_Records;
      Text_File : File_Type;

      procedure Check_Delimiter is

         Ch : Character;

      begin -- Check_Delimiter
         Get (Text_File, Ch);
         if Ch /= Delimiter then
            raise Brightness_Error with
              "Found '" & Ch & "'" & " expected " & Delimiter;
         end if; -- Ch /= Delimiter
      end Check_Delimiter;

   begin -- Read_Brightness_Config
      begin -- open current file
         Open (Text_File, In_file, File_Name & Current_Extension);
      exception
         when others =>
            begin -- rename and open backup file
               Rename (File_Name & Backup_Extension,
                       File_Name & Current_Extension);
               -- This could fail for reasons other than the file not existing;
               -- however if the file is locked Open will probably fail. In any
               -- case an exception will be reised.
               Open (Text_File, In_file, File_Name & Current_Extension);
            exception
               when Event: others =>
                  Put_Error ("Unable to read " & File_Name & Current_Extension &
                              " or " & File_Name & Backup_Extension, Event);
                  Put_Line (Exception_Name(Event));
                  raise;
            end; -- rename and open backup file
      end; -- open current file

      begin -- Read Data
         Lit_Greyscale_IO.Get (Text_File, Result.Minimum_Brightness);
         Check_Delimiter;
         Greyscale_IO.Get (Text_File, Result.Chime_Brightness);
         Check_Delimiter;
         Gamma_IO.Get (Text_File, Result.Gamma);
         Skip_Line (Text_File);
         while not End_Of_File (Text_File) loop
            -- read one correction
            LED_Driver_IO.Get (Text_File, LED_Driver);
            Check_Delimiter;
            LED_Channel_IO.Get (Text_File, LED_Channel);
            Check_Delimiter;
            Correction_IO.Get (Text_File, Result.
                                 Dot_Correction (LED_Driver, LED_Channel));
            Skip_Line (Text_File);
         end loop; -- End_Of_File (Text_File)
      exception
         when Event: others =>
            Put_Error ("Bad brightness data at line:" &
                        Positive_Count'Image (Line (Text_File)), Event);
            Put_Line (Exception_Name (Event));
            Put (Exception_Message (Event));
            raise;
      end; -- Read Data
      Close (Text_File);
      return Result;
   end Read_Brightness_Config;

   procedure Write_Brightness_Config
     (Brighness_Record : in Brighness_Records) is

      Text_File : File_Type;

   begin -- Write_Brightness_Config
      Create (Text_File, Out_File, File_Name & Temporary_Extension);
      Lit_Greyscale_IO.Put (Text_File, Brighness_Record.Minimum_Brightness, 4);
      Put (Text_File, Delimiter);
      Greyscale_IO.Put (Text_File, Brighness_Record.Chime_Brightness, 5);
      Put (Text_File, Delimiter);
      Gamma_IO.Put (Text_File, Brighness_Record.Gamma);
      New_Line (Text_File);
      for LED_Driver in LED_Drivers loop
         for LED_Channel in LED_Channels loop
            LED_Driver_IO.Put (Text_File, LED_Driver);
            Put (Text_File, Delimiter);
            LED_Channel_IO.Put (Text_File, LED_Channel, 3);
            Put (Text_File, Delimiter);
            Correction_IO.Put (Text_File, Brighness_Record.Dot_Correction
                               (LED_Driver, LED_Channel));
            New_Line (Text_File);
         end loop; --  LED_Channel in LED_Channels
      end loop; -- LED_Driver in LED_Drivers
      Close (Text_File);
      if Exists (File_Name & Backup_Extension) then
         Delete_File (File_Name & Backup_Extension);
      end if; --  Exists (File_Name & Backup_Extension)
      if Exists (File_Name & Current_Extension) then
         Rename (File_Name & Current_Extension, File_Name & Backup_Extension);
      end if; -- Exists (File_Name & Current_Extension)
      Rename (File_Name & Temporary_Extension, File_Name & Current_Extension);
   end Write_Brightness_Config;

end Brightness;
