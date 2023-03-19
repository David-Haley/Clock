-- Provides for chiming on the hour.A sound file is required for each hour the
-- clock is required to chime. The configuration file containing one line for
-- each hour that the clock is required to chime. Up to 24 line are read. If
-- an entry for an hour is missing no chime is played. If the configuration file
-- is absent chiming is disabled.

-- Author    : David Haley
-- Created   : 28/03/2019
-- Last Edit : 25/01/2022

-- 20220125 : Get volume and Get_Chiming removed from public interface, no
-- longer used, replaced by reporting to User_Interface_Server.
-- 20220120 : Test_Volume added.
-- 20220116 : Raise_Volume, Lower_Volume and Get_Volume added
-- 20190730 : Functional decription improved.
-- 20190707 : Spelling of Chiming corrected.

with Shared_User_Interface; use Shared_User_Interface;

package Chime is

   procedure Chiming;
   -- Enables chiming

   procedure Silent;
   -- Disables Chiming

   procedure Raise_Volume;
   -- Raises volume 1% for each call

   procedure Lower_Volume;
   -- Lowers volume 1% for each call

   procedure Test_Volume;
   -- Uses aplay to play volume_test.wav from directory from which the
   -- appliction was started.

   procedure End_Chiming;
   -- End Chiming process to allow main application to exit

end Chime;
