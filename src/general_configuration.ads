-- Author    : David Haley
-- Created   : 05/04/2025
-- Last Edit : 06/04/202
-- This package reads the general clock confifuration;

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

end General_Configuration;
