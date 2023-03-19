-- Author    : David Haley
-- Created   : 01/07/2019
-- Last Edit : 09/06/2022
-- This package manages the Clock Brightness configuration file.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20190722 : Types Real and Gammas added.
-- 20190717 :  Lit_Greyscales spelling corrected
-- 20190715 :Eexceptions removed

with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;

package Brightness is

   Default_Correction : constant Corrections := 31;

   type Dot_Corrections is array (LED_Drivers, LED_Channels) of Corrections;

   subtype Lit_Greyscales is Greyscales range 1 .. Greyscales'Last;

   type Real is digits 15;
   subtype Gammas is Real range 1.0 .. 4.0;

   type Brighness_Records is record
      Minimum_Brightness : Lit_Greyscales := Lit_Greyscales'First;
      Chime_Brightness : Greyscales := Greyscales'First;
      Gamma : Gammas := 2.5;
      Dot_Correction : Dot_Corrections :=
        (Sweep_00_14 => (others => Default_Correction),
         Sweep_15_29 => (others => Default_Correction),
         Sweep_30_44 => (others => Default_Correction),
         Sweep_45_59 => (others => Default_Correction),
         Seconds_Drv => (others => Default_Correction),
         Minutes_Drv => (others => Default_Correction),
         Hours_Drv => (others => Default_Correction),
         Years_Drv => (others => Default_Correction),
         Months_Drv => (others => Default_Correction),
         Days_Drv => (others => Default_Correction));
   end record; -- Brightness_Records

   function Read_Brightness_Config return Brighness_Records;

   procedure Write_Brightness_Config (Brighness_Record : in Brighness_Records);

end Brightness;
