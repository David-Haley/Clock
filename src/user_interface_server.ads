-- This package provides server component for a locally distributed IOT_Clock
-- user interface.
-- Author    : David Haley
-- Created   : 24/07/2019
-- Last Edit : 10/05/2025

-- 20250510 : Provision for multiple simulated sweep hand modes.
-- 20220920 :  User initiated shutdown moved to main loop.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220125 : Provision for reporting to the user interface and local storage to
-- improve efficiency of handling multiple cliants.
-- 20220119 : Get_Chime_Toggle added.
-- 20220118 : Decoupling user interface from main loop timing;
-- 20220116 : Implementation of chiming control via user 1nterface.
-- 20190726 : Dependency on Secondary_Dislpay removed

with Ada.Calendar; use Ada.Calendar;
with Shared_User_Interface; use Shared_User_Interface;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;

package User_Interface_Server is

   function Get_Chime_Toggle return Boolean;
   -- Returns true if chiming is turned on in UI, defaults to true on startup.

   procedure Report_Chiming (Chiming_Enabled : in Boolean;
                             Chime_Volume : in Chime_Volumes);
   -- Provides for reporting of current chime state to user interface.

   procedure Report_Time (Current_Time : in Time);
   -- Provides for reporting of time to the User Interface.

   procedure Report_Current_Item (Current_Item : UI_Strings);
   -- Provides for reporting of the item being displayed on the secondary
   -- display.

   procedure Report_Ambient_Light (Ambient_Light,
                                   AL_Test_Value : in Greyscales);
   -- Provides for reporting of current ambient light and the current
   -- comparison value to user interface.

   procedure Report_LED (Driver : in LED_Drivers;
                         Channel : in LED_Channels;
                         Greyscale : in Greyscales);
   -- Provides for reporting of LED state  to the User Interface.

   procedure Stop_UI_Server;

   -- Terminate user interface server

end User_Interface_Server;
