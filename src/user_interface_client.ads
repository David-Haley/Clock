-- This package provides the client component for a locally distibuted Clock
-- user interface.
-- Author    : David Haley
-- Created   : 25/07/2019
-- Last Edit : 25/05/2026

--  20260525 : Compiler warnings removed.
-- 20220126 : Version_Mismatch Exception removed.
-- 20220116 : made generic, chiming controls added.
-- 20190729 : Repeat_Statue added

generic

   Clock_Name : String;

package User_Interface_Client is

   procedure Run_UI;

end User_Interface_Client;
