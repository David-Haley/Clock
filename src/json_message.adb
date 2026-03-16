-- Lightweight JSON parser for the fixed MQTT display message schema.
--
-- Author    : David Haley
-- Created   : 17/03/2026

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body JSON_Message is

   -- Returns True and sets Value if key "Key" has a string value in JSON.
   function Find_String_Value (JSON    : in  String;
                                Key     : in  String;
                                Value   : out Unbounded_String)
                                return Boolean is

      Key_Pattern : constant String := """" & Key & """";
      Key_Pos     : Natural;
      Colon_Pos   : Natural;
      Open_Pos    : Natural;
      Close_Pos   : Natural;

   begin -- Find_String_Value
      Key_Pos := Index (JSON, Key_Pattern);
      if Key_Pos = 0 then
         return False;
      end if; -- Key_Pos = 0
      Colon_Pos := Index (JSON, ":", Key_Pos + Key_Pattern'Length);
      if Colon_Pos = 0 then
         return False;
      end if; -- Colon_Pos = 0
      Open_Pos := Index (JSON, """", Colon_Pos + 1);
      if Open_Pos = 0 then
         return False;
      end if; -- Open_Pos = 0
      Close_Pos := Index (JSON, """", Open_Pos + 1);
      if Close_Pos = 0 then
         return False;
      end if; -- Close_Pos = 0
      Value := To_Unbounded_String (JSON (Open_Pos + 1 .. Close_Pos - 1));
      return True;
   end Find_String_Value;

   -- Returns True and sets Value if key "Key" has a non-negative integer value.
   function Find_Integer_Value (JSON  : in  String;
                                 Key   : in  String;
                                 Value : out Natural)
                                 return Boolean is

      Key_Pattern  : constant String := """" & Key & """";
      Key_Pos      : Natural;
      Colon_Pos    : Natural;
      Digit_Start  : Natural;
      Digit_End    : Natural;

   begin -- Find_Integer_Value
      Key_Pos := Index (JSON, Key_Pattern);
      if Key_Pos = 0 then
         return False;
      end if; -- Key_Pos = 0
      Colon_Pos := Index (JSON, ":", Key_Pos + Key_Pattern'Length);
      if Colon_Pos = 0 then
         return False;
      end if; -- Colon_Pos = 0
      Digit_Start := Colon_Pos + 1;
      -- Skip whitespace
      while Digit_Start <= JSON'Last and then JSON (Digit_Start) = ' ' loop
         Digit_Start := Digit_Start + 1;
      end loop; -- whitespace skip
      if Digit_Start > JSON'Last or else
        JSON (Digit_Start) not in '0' .. '9' then
         return False;
      end if; -- no digit found
      Digit_End := Digit_Start;
      while Digit_End < JSON'Last and then
        JSON (Digit_End + 1) in '0' .. '9' loop
         Digit_End := Digit_End + 1;
      end loop; -- scan digits
      Value := Natural'Value (JSON (Digit_Start .. Digit_End));
      return True;
   exception
      when others =>
         return False;
   end Find_Integer_Value;

   -- Returns True and sets Value if key "Key" has a boolean value.
   function Find_Boolean_Value (JSON  : in  String;
                                 Key   : in  String;
                                 Value : out Boolean)
                                 return Boolean is

      Key_Pattern : constant String := """" & Key & """";
      Key_Pos     : Natural;
      Colon_Pos   : Natural;
      Val_Start   : Natural;

   begin -- Find_Boolean_Value
      Key_Pos := Index (JSON, Key_Pattern);
      if Key_Pos = 0 then
         return False;
      end if; -- Key_Pos = 0
      Colon_Pos := Index (JSON, ":", Key_Pos + Key_Pattern'Length);
      if Colon_Pos = 0 then
         return False;
      end if; -- Colon_Pos = 0
      Val_Start := Colon_Pos + 1;
      -- Skip whitespace
      while Val_Start <= JSON'Last and then JSON (Val_Start) = ' ' loop
         Val_Start := Val_Start + 1;
      end loop; -- whitespace skip
      if Val_Start + 3 <= JSON'Last and then
        JSON (Val_Start .. Val_Start + 3) = "true" then
         Value := True;
         return True;
      elsif Val_Start + 4 <= JSON'Last and then
        JSON (Val_Start .. Val_Start + 4) = "false" then
         Value := False;
         return True;
      else
         return False;
      end if; -- true/false check
   end Find_Boolean_Value;

   procedure Parse_Display_Message (JSON     : in  String;
                                    Priority : in  Priority_Levels;
                                    Msg      : out Display_Messages;
                                    Success  : out Boolean) is

      Text_Val   : Unbounded_String;
      Int_Val    : Natural;
      Bool_Val   : Boolean;

   begin -- Parse_Display_Message
      Msg := (Priority => Priority, others => <>);
      if not Find_String_Value (JSON, "text", Text_Val) then
         Success := False;
         return;
      end if; -- text field missing
      Msg.Text := Text_Val;
      if Find_Integer_Value (JSON, "repeat", Int_Val) then
         Msg.Repeat_Count := Int_Val;
      end if; -- repeat found
      if Find_Integer_Value (JSON, "delay_ms", Int_Val) then
         Msg.Scroll_Delay_Ms := Int_Val;
      end if; -- delay_ms found
      if Find_Boolean_Value (JSON, "force_uppercase", Bool_Val) then
         Msg.Force_Uppercase := Bool_Val;
      end if; -- force_uppercase found
      Success := True;
   end Parse_Display_Message;

end JSON_Message;
