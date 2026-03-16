-- MQTT subscriber tasks for the secondary display message queue.
-- Subscribes to three topics (Normal, High, Critical priority).
-- Received JSON payloads are parsed and enqueued for scroll display.
--
-- Author    : David Haley
-- Created   : 17/03/2026

package MQTT_Display is

   Normal_Topic   : constant String := "clock/display/normal";
   High_Topic     : constant String := "clock/display/high";
   Critical_Topic : constant String := "clock/display/critical";

   procedure Start_MQTT_Display;
   -- Reads broker config from General_Configuration, connects all three
   -- MQTT subscribers, and starts their receiver tasks.
   -- If MQTT_Broker_Host is empty in General_Configuration, this is a no-op.

   procedure Stop_MQTT_Display;
   -- Disconnects all three subscribers and aborts their receiver tasks.

end MQTT_Display;
