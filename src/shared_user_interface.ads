-- This package provides the shared information to support a distributed user
-- interface.

-- Author    : David Haley
-- Created   : 24/07/2019
-- Last Edit : 10/05/2025

-- 20250510 : Provision for multiple simulated sweep hand modes.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types. Stop_Clock removed.
-- 20220126 : Diagnostic_Toggle addded to Request_Records and Status_Records.
-- 20220125 : Status_Records no longer variant, Null_Request removed. User
-- interface only stores lit state od LEDs.
-- 20220122 : Volume_Test added;
-- 20220119 : UI_Chime_Toggle added.
-- 20220116 : Controlling chiming via user interface implemented.
-- 20180726 : Dependency on Secondary_Display removed to prevent instantiation
-- of TLC5940 in Clock_UI

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Sockets; use GNAT.Sockets;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;

package Shared_User_Interface is

   subtype Chime_Volumes is Natural range 0 .. 100;

   Interface_Version : constant Version_String := "20250510";
   Request_Port : Port_Type := 50003;
   Response_Port : Port_Type := Request_Port + 1;

   type Requests is (Toggle_Chime, Cycle_Sweep, Volume_Up, Volume_Down,
                     Volume_Test, Get_Status, Exit_User_Interface);

   type Request_Records is record
      User_Interface_Version : Version_String := Interface_Version;
      Request : Requests;
      Diagnostic_Toggle : Boolean := False;
   end record; -- Request_Records

   subtype UI_Strings is String (1 .. 80);
   type LED_Arrays is array (LED_Drivers, LED_Channels) of Boolean;
   -- Record lit state only

   type Status_Records is record
      Clock_Version : Version_String := "YYYYMMDD";
      Request : Requests := Get_Status;
      Diagnostic_Toggle : Boolean := False;
      User_Interface_Version : Version_String := Interface_Version;
      Current_Time : Time;
      Chime_Enabled : Boolean := True;
      Chime_Toggle : Boolean := True;
      Chime_Volume : Chime_Volumes := Chime_Volumes'First;
      Ambient_Light, AL_Test_Value : Greyscales := Greyscales'First;
      Current_Item : UI_Strings := (others => ' ');
      LED_Array : LED_Arrays := (others => (others => False));
   end record; -- Status_Records
   -- All but Current_Time initialised to ensure legal values for Update_Screen
   -- in User_Interface_Client

   subtype Request_Buffers is Stream_Element_Array
     (1 .. (Request_Records'Size+7) / Stream_Element'Size);
   subtype Response_Buffers is Stream_Element_Array
     (1 .. (Status_Records'Size + 7) / Stream_Element'Size);

end Shared_User_Interface;
