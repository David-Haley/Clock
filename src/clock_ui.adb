-- This program is the client that provides the user interface for the
-- IOT-Clock.

-- Author    : David Haley
-- Created   : 24/07/2019
-- Last Edit : 20/09/2022

-- 20220920 Shared_User_Interface is not directly used.
-- 20220123 : Full rework clock animation added.
-- 20220121 : Enhanced Windows performance.
-- 20220120 : Chiming controls including Test volume functional;
-- 20200824 : User interface Chiming control implemented

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with User_Interface_Client;

procedure Clock_UI is

   procedure Process_Commands (Clock_Name : String) is

      package User_Interface is new User_Interface_Client (Clock_Name);

   begin -- Process_Commands
      User_Interface.Run_UI;
   end Process_Commands;

begin -- Clock_UI
   if Argument_Count = 1 then
      Process_Commands (Argument (1));
   else
      Process_Commands ("IOT-Clock");
   end if; -- Argument_Count = 1
exception
   when Event: others =>
      Put_Line ("Clock_UI - " & Exception_Name (Event) & " - " &
                  Exception_Message (Event));
      raise;
end Clock_UI;
