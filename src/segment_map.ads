-- Provides character-to-7-segment mapping for the secondary display.
-- Ported from text2Arb.py (David Haley). Segment indices:
--   a=0, b=1, c=2, d=3, e=4, f=5, g=6, DP=7
--
-- Author    : David Haley (Ada port)
-- Created   : 17/03/2026

with TLC5940_Driver_Types; use TLC5940_Driver_Types;

package Segment_Map is

   type Segment_Sets is array (Segments) of Boolean;

   Blank_Segments : constant Segment_Sets := (others => False);

   function Char_To_Segments (C           : Character;
                               Force_Upper : Boolean := True)
                               return Segment_Sets;
   -- Returns the segment pattern for C.
   -- If Force_Upper is True, lowercase letters are converted to their
   -- uppercase representation before lookup (improves readability on 7-segment
   -- displays where some lowercase glyphs are hard to distinguish).
   -- Characters with no defined pattern return Blank_Segments.

end Segment_Map;
