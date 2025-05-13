-- This package reads the general clock confifuration;

-- Author    : David Haley
-- Created   : 05/04/2025
-- Last Edit : 13/05/2025

-- 20250513: Smooth added as a simulated sweep mode.
-- 20250510: Providing for simulated sweep hand modes to be set on startup.
-- 20250410: Read_General_Configuration, Play_Command and Volume_Command added.

with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with Shared_User_Interface; use Shared_User_Interface;

package General_Configuration is

   type Real is digits 15;

   subtype Gammas is Real range 1.0 .. 4.0;
   
   type Sweep_Modes is (Normal, Smooth, With_Tail, Sub_Second);

   function Minimum_Brightness return Lit_Greyscales;
   -- Returns the minimum dislpay brightness

   function Minimum_Chime return Greyscales;
   -- Returns mimimum brightness level at which chiming should occur.

   function Sweep_Mode return Sweep_Modes;
   -- Returns the startup mode ot the simulated sweep hand.
      
   procedure Cycle_Sweep_Mode;
   -- Cycles through sweep modes, stepping to a new mode each time it is
   -- called.

   function Gamma return Gammas;
   -- Returns the gamma value to be applied to the leading edge LED of the
   -- simulated sweep hand, only applies to Smooth and With_Tail modes.

   function Default_Volume return Chime_Volumes;
   -- The initial chime volume of the clock when started.

   function Play_Command return String;
   -- Returns the command line string used to invoke the sound player
   -- application.

   function Volume_Command return String;
   -- Returns the command line string used to invoke the volume control
   -- appliation.

end General_Configuration;
