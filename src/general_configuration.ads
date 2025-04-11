-- This package reads the general clock confifuration;

-- Author    : David Haley
-- Created   : 05/04/2025
-- Last Edit : 10/04/2025

-- 202504010: Read_General_Configuration, Play_Command and Volume_Command added.

with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with Shared_User_Interface; use Shared_User_Interface;

package General_Configuration is

   type Real is digits 15;

   subtype Gammas is Real range 1.0 .. 4.0;

   function Minimum_Brightness return Lit_Greyscales;
   -- Returns the minimum dislpay brightness

   function Minimum_Chime return Greyscales;
   -- Returns mimimum brightness level at which chiming should occur.

   function Gamma return Gammas;
   -- Returns the gamma value to be applied to the leading edge LED of the
   -- simulated sweep hand.

   function Default_Volume return Chime_Volumes;
   -- The initial chime volume of the clock when started.

   function Play_Command return String;
   -- Returns the command line string used to invoke the sound player
   -- application.

   function Volume_Command return String;
   -- Returns the command line string used to invoke the volume control
   -- appliation.

end General_Configuration;
