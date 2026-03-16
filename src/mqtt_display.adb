-- MQTT subscriber tasks for the secondary display message queue.
--
-- Author    : David Haley
-- Created   : 17/03/2026

with MQTT.Subscriber;
with MQTT;
with General_Configuration;
with Display_Message_Queue; use Display_Message_Queue;
with JSON_Message; use JSON_Message;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Ada.Exceptions; use Ada.Exceptions;

package body MQTT_Display is

   -- Three independent MQTT.Subscriber instantiations, one per priority topic.
   package Normal_Sub   is new MQTT.Subscriber (Normal_Topic);
   package High_Sub     is new MQTT.Subscriber (High_Topic);
   package Critical_Sub is new MQTT.Subscriber (Critical_Topic);

   -- Receiver tasks. Each blocks on its subscriber's Receive call then
   -- parses the JSON payload and enqueues the result.
   -- Tasks wait on accept Start before entering their receive loop, so they
   -- do not block at elaboration time. The select...or terminate alternative
   -- allows clean shutdown if Start_MQTT_Display is never called (e.g. when
   -- MQTT_Broker_Host is empty and MQTT is disabled).

   task Normal_Task is
      entry Start;
   end Normal_Task;

   task High_Task is
      entry Start;
   end High_Task;

   task Critical_Task is
      entry Start;
   end Critical_Task;

   -- Common receive loop body, parameterised by subscriber package and
   -- priority. Written as separate task bodies (not a generic task) because
   -- Ada tasks cannot be directly parameterised by a package.

   task body Normal_Task is

      Msg     : Display_Messages;
      OK      : Boolean;

   begin -- Normal_Task
      select
         accept Start;
      or
         terminate;
      end select;
      loop
         declare
            Payload : constant String := Normal_Sub.Receive;
         begin
            Parse_Display_Message (Payload, Normal, Msg, OK);
            if OK then
               Enqueue (Msg);
            else
               Put_Error ("MQTT Normal: bad JSON payload: " & Payload,
                          "MQTT_Display");
            end if; -- OK
         end;
      end loop;
   exception
      when others => null; -- exit cleanly after Disconnect or on error
   end Normal_Task;

   task body High_Task is

      Msg     : Display_Messages;
      OK      : Boolean;

   begin -- High_Task
      select
         accept Start;
      or
         terminate;
      end select;
      loop
         declare
            Payload : constant String := High_Sub.Receive;
         begin
            Parse_Display_Message (Payload, High, Msg, OK);
            if OK then
               Enqueue (Msg);
            else
               Put_Error ("MQTT High: bad JSON payload: " & Payload,
                          "MQTT_Display");
            end if; -- OK
         end;
      end loop;
   exception
      when others => null;
   end High_Task;

   task body Critical_Task is

      Msg     : Display_Messages;
      OK      : Boolean;

   begin -- Critical_Task
      select
         accept Start;
      or
         terminate;
      end select;
      loop
         declare
            Payload : constant String := Critical_Sub.Receive;
         begin
            Parse_Display_Message (Payload, Critical, Msg, OK);
            if OK then
               Enqueue (Msg);
            else
               Put_Error ("MQTT Critical: bad JSON payload: " & Payload,
                          "MQTT_Display");
            end if; -- OK
         end;
      end loop;
   exception
      when others => null;
   end Critical_Task;

   procedure Start_MQTT_Display is

      Broker : constant String  := General_Configuration.MQTT_Broker_Host;
      User   : constant String  := General_Configuration.MQTT_User_Name;
      Passwd : constant String  := General_Configuration.MQTT_Password;
      QoS    : constant MQTT.QoSs :=
        MQTT.QoSs (General_Configuration.MQTT_QoS);
      KA     : constant MQTT.Keep_Alive_Times :=
        MQTT.Keep_Alive_Times (General_Configuration.MQTT_Keep_Alive);

   begin -- Start_MQTT_Display
      if Broker = "" then
         Put_Event ("MQTT_Broker_Host not configured; MQTT display disabled",
                    "MQTT_Display");
         return;
      end if; -- Broker empty
      Normal_Sub.Connect (Broker, User, Passwd, QoS, KA);
      High_Sub.Connect (Broker, User, Passwd, QoS, KA);
      Critical_Sub.Connect (Broker, User, Passwd, QoS, KA);
      Normal_Task.Start;
      High_Task.Start;
      Critical_Task.Start;
      Put_Event ("MQTT display connected to " & Broker, "MQTT_Display");
   exception
      when E : others =>
         Put_Error ("MQTT_Display.Start: " & Exception_Message (E),
                    "MQTT_Display");
   end Start_MQTT_Display;

   procedure Stop_MQTT_Display is

   begin -- Stop_MQTT_Display
      -- Abort tasks first so they stop blocking on Receive (which would
      -- block forever after Disconnect stops the libmosquitto loop).
      abort Normal_Task;
      abort High_Task;
      abort Critical_Task;
      -- Disconnect from broker and release C library resources.
      begin
         Normal_Sub.Disconnect;
      exception
         when others => null;
      end;
      begin
         High_Sub.Disconnect;
      exception
         when others => null;
      end;
      begin
         Critical_Sub.Disconnect;
      exception
         when others => null;
      end;
   end Stop_MQTT_Display;

end MQTT_Display;
