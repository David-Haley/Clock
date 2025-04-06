-- Author    : David Haley
-- Created   : 01/07/2019
-- Last Edit : 05/04/2025
-- This package manages the Clock Brightness configuration file.
-- 20250405 : Minimum_Brightness, Chime_Brightness and Gamma removed to gereral
-- configuration. DJH.Parse_CSV used to read in corrections. Brightness Records
-- Removed.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20190722 : Types Real and Gammas added.
-- 20190717 :  Lit_Greyscales spelling corrected
-- 20190715 :Eexceptions removed

with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;

package Brightness is

   type Dot_Corrections is array (LED_Drivers, LED_Channels) of Corrections;

   function Read_Brightness_Config return Dot_Corrections;
   -- Reads dot corrections from CSV file.

   procedure Write_Brightness_Config (Dot_Correction : in Dot_Corrections);
   -- Writes dot correction file.

end Brightness;
