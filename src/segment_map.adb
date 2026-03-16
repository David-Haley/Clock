-- Provides character-to-7-segment mapping for the secondary display.
-- Ported from text2Arb.py (David Haley). Mappings largely adapted from
-- https://en.wikipedia.org/wiki/Seven-segment_display_character_representations
--
-- Author    : David Haley (Ada port)
-- Created   : 17/03/2026

package body Segment_Map is

   function Char_To_Segments (C           : Character;
                               Force_Upper : Boolean := True)
                               return Segment_Sets is

      -- If Force_Upper requested, fold lowercase to uppercase before lookup.
      Actual : constant Character :=
        (if Force_Upper and then C in 'a' .. 'z'
         then Character'Val (Character'Pos (C) - 32)
         else C);

   begin -- Char_To_Segments
      case Actual is

         -- Digits
         when '0' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_e | Segment_f => True, others => False);
         when '1' =>
            return (Segment_b | Segment_c => True, others => False);
         when '2' =>
            return (Segment_a | Segment_b | Segment_d | Segment_e |
                    Segment_g => True, others => False);
         when '3' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_g => True, others => False);
         when '4' =>
            return (Segment_b | Segment_c | Segment_f | Segment_g => True,
                    others => False);
         when '5' =>
            return (Segment_a | Segment_c | Segment_d | Segment_f |
                    Segment_g => True, others => False);
         when '6' =>
            return (Segment_a | Segment_c | Segment_d | Segment_e |
                    Segment_f | Segment_g => True, others => False);
         when '7' =>
            return (Segment_a | Segment_b | Segment_c => True,
                    others => False);
         when '8' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_e | Segment_f | Segment_g => True,
                    others => False);
         when '9' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_f | Segment_g => True, others => False);

         -- Uppercase letters (also reached from lowercase when Force_Upper=True)
         when 'A' =>
            return (Segment_a | Segment_b | Segment_c | Segment_e |
                    Segment_f | Segment_g => True, others => False);
         when 'B' =>
            -- Same as 8
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_e | Segment_f | Segment_g => True,
                    others => False);
         when 'C' =>
            return (Segment_a | Segment_d | Segment_e | Segment_f => True,
                    others => False);
         when 'D' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_e => True, others => False);
         when 'E' =>
            return (Segment_a | Segment_d | Segment_e | Segment_f |
                    Segment_g => True, others => False);
         when 'F' =>
            return (Segment_a | Segment_e | Segment_f | Segment_g => True,
                    others => False);
         when 'G' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_f => True, others => False);
         when 'H' =>
            return (Segment_b | Segment_c | Segment_e | Segment_f |
                    Segment_g => True, others => False);
         when 'I' =>
            return (Segment_e | Segment_f => True, others => False);
         when 'J' =>
            return (Segment_b | Segment_c | Segment_d => True,
                    others => False);
         when 'K' =>
            return (Segment_a | Segment_c | Segment_e | Segment_f |
                    Segment_g => True, others => False);
         when 'L' =>
            return (Segment_d | Segment_e | Segment_f => True,
                    others => False);
         when 'M' =>
            return (Segment_a | Segment_b | Segment_d | Segment_f => True,
                    others => False);
         when 'N' =>
            return (Segment_a | Segment_b | Segment_c | Segment_e |
                    Segment_f => True, others => False);
         when 'O' =>
            -- Same as 0
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_e | Segment_f => True, others => False);
         when 'P' =>
            return (Segment_a | Segment_b | Segment_e | Segment_f |
                    Segment_g => True, others => False);
         when 'Q' =>
            return (Segment_a | Segment_b | Segment_d | Segment_f |
                    Segment_g => True, others => False);
         when 'R' =>
            return (Segment_a | Segment_b | Segment_d | Segment_e |
                    Segment_f | Segment_g => True, others => False);
         when 'S' =>
            -- Same as 5
            return (Segment_a | Segment_c | Segment_d | Segment_f |
                    Segment_g => True, others => False);
         when 'T' =>
            return (Segment_a | Segment_e | Segment_f => True,
                    others => False);
         when 'U' =>
            return (Segment_b | Segment_c | Segment_d | Segment_e |
                    Segment_f => True, others => False);
         when 'V' =>
            return (Segment_b | Segment_c | Segment_d | Segment_f => True,
                    others => False);
         when 'W' =>
            return (Segment_a | Segment_c | Segment_d | Segment_e => True,
                    others => False);
         when 'X' =>
            return (Segment_a | Segment_d | Segment_g => True,
                    others => False);
         when 'Y' =>
            return (Segment_b | Segment_d | Segment_f | Segment_g => True,
                    others => False);
         when 'Z' =>
            -- Same as 2
            return (Segment_a | Segment_b | Segment_d | Segment_e |
                    Segment_g => True, others => False);

         -- Lowercase letters (only reached when Force_Upper = False)
         when 'a' =>
            return (Segment_c | Segment_d | Segment_g => True,
                    others => False);
         when 'b' =>
            return (Segment_c | Segment_d | Segment_e | Segment_f |
                    Segment_g => True, others => False);
         when 'c' =>
            return (Segment_d | Segment_e | Segment_g => True,
                    others => False);
         when 'd' =>
            return (Segment_b | Segment_c | Segment_d | Segment_e |
                    Segment_g => True, others => False);
         when 'e' =>
            return (Segment_d | Segment_e => True, others => False);
         when 'f' =>
            return (Segment_e | Segment_f | Segment_g => True,
                    others => False);
         when 'g' =>
            return (Segment_a | Segment_d | Segment_e | Segment_g => True,
                    others => False);
         when 'h' =>
            return (Segment_c | Segment_e | Segment_f | Segment_g => True,
                    others => False);
         when 'i' =>
            return (Segment_a | Segment_d | Segment_e => True,
                    others => False);
         when 'j' =>
            return (Segment_a | Segment_c | Segment_d => True,
                    others => False);
         when 'k' =>
            return (Segment_a | Segment_d | Segment_f | Segment_g => True,
                    others => False);
         when 'l' =>
            return (Segment_e | Segment_f => True, others => False);
         when 'm' =>
            return (Segment_a | Segment_c | Segment_e | Segment_g => True,
                    others => False);
         when 'n' =>
            return (Segment_c | Segment_e | Segment_g => True,
                    others => False);
         when 'o' =>
            return (Segment_c | Segment_d | Segment_e | Segment_g => True,
                    others => False);
         when 'p' =>
            -- Same as P
            return (Segment_a | Segment_b | Segment_e | Segment_f |
                    Segment_g => True, others => False);
         when 'q' =>
            return (Segment_a | Segment_b | Segment_c | Segment_f |
                    Segment_g => True, others => False);
         when 'r' =>
            return (Segment_e | Segment_g => True, others => False);
         when 's' =>
            return (Segment_c | Segment_d => True, others => False);
         when 't' =>
            return (Segment_d | Segment_e | Segment_f | Segment_g => True,
                    others => False);
         when 'u' =>
            return (Segment_c | Segment_d | Segment_e => True,
                    others => False);
         when 'v' =>
            return (Segment_c | Segment_d => True, others => False);
         when 'w' =>
            return (Segment_b | Segment_d | Segment_f => True,
                    others => False);
         when 'x' =>
            return (Segment_d | Segment_g => True, others => False);
         when 'y' =>
            return (Segment_b | Segment_c | Segment_d | Segment_f |
                    Segment_g => True, others => False);
         when 'z' =>
            return (Segment_d | Segment_g => True, others => False);

         -- Space
         when ' ' =>
            return Blank_Segments;

         -- Punctuation
         when '.' =>
            return (Segment_DP => True, others => False);
         when '?' =>
            return (Segment_a | Segment_b | Segment_DP => True,
                    others => False);
         when '!' =>
            return (Segment_b | Segment_c | Segment_DP => True,
                    others => False);
         when ',' =>
            return (Segment_d | Segment_DP => True, others => False);
         when ';' =>
            return (Segment_b | Segment_d | Segment_DP => True,
                    others => False);
         when ':' =>
            return (Segment_b | Segment_DP => True, others => False);
         when '"' =>
            return (Segment_b | Segment_f => True, others => False);
         when ''' =>
            return (Segment_f => True, others => False);
         when '`' =>
            return (Segment_f => True, others => False);
         when '-' =>
            return (Segment_g => True, others => False);
         when '_' =>
            return (Segment_d => True, others => False);
         when '=' =>
            return (Segment_d | Segment_g => True, others => False);
         when '(' | '[' =>
            return (Segment_a | Segment_d | Segment_e | Segment_f |
                    Segment_DP => True, others => False);
         when ')' | ']' =>
            return (Segment_a | Segment_b | Segment_c | Segment_d |
                    Segment_DP => True, others => False);

         when others =>
            return Blank_Segments;

      end case; -- Actual
   end Char_To_Segments;

end Segment_Map;
