-- This package provides the client component for a locally distibuted Clock
-- user interface.
-- Author    : David Haley
-- Created   : 25/07/2019
-- Last Edit : 16/01/2022

-- 20220126 : Version_Mismatch Exception removed.
-- 20220116 : made generic, chiming controls added.
-- 20190729 : Repeat_Statue added

with Shared_User_Interface; use Shared_User_Interface;

generic

   Clock_Name : String;

package User_Interface_Client is

   procedure Run_UI;

end User_Interface_Client;
