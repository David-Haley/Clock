-- Lightweight JSON parser for the fixed MQTT display message schema.
-- No external JSON library required; scans for known keys directly.
--
-- Expected payload format (all fields except "text" are optional):
--   {"text": "HOT WATER LOW", "repeat": 3, "delay_ms": 500,
--    "force_uppercase": true}
--
-- Author    : David Haley
-- Created   : 17/03/2026

with Display_Message_Queue; use Display_Message_Queue;

package JSON_Message is

   procedure Parse_Display_Message (JSON     : in  String;
                                    Priority : in  Priority_Levels;
                                    Msg      : out Display_Messages;
                                    Success  : out Boolean);
   -- Parses JSON and populates Msg.
   -- Priority is determined by the MQTT topic and passed in by the caller.
   -- Sets Success=False if "text" field is absent or JSON is malformed.
   -- Unknown JSON fields are silently ignored.

end JSON_Message;
