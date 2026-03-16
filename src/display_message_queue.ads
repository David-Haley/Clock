-- Priority message queue for the secondary display.
-- Three FIFO queues (Normal, High, Critical) allow preemption semantics.
-- Thread-safe: MQTT subscriber tasks write, main loop reads.
--
-- Author    : David Haley
-- Created   : 17/03/2026

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Display_Message_Queue is

   type Priority_Levels is (Normal, High, Critical);

   type Display_Messages is record
      Text            : Unbounded_String := Null_Unbounded_String;
      Priority        : Priority_Levels  := Normal;
      Scroll_Delay_Ms : Natural          := 0;
      -- 0 = use the Scroll_Delay_Ms value from General_Configuration
      Repeat_Count    : Natural          := 1;
      -- 0 = loop forever; N > 0 = display exactly N times
      Force_Uppercase : Boolean          := True;
   end record; -- Display_Messages

   procedure Enqueue (Msg : in Display_Messages);
   -- Enqueues Msg into the FIFO queue for Msg.Priority.

   procedure Dequeue_Next (Msg   : out Display_Messages;
                           Found : out Boolean);
   -- Checks Critical queue first, then High, then Normal.
   -- Sets Found=True and populates Msg if any queue is non-empty.
   -- Sets Found=False if all queues are empty.

   function Has_Critical return Boolean;
   -- True if the Critical queue is non-empty.

   function Has_High return Boolean;
   -- True if the High queue is non-empty.

   function Any_Pending return Boolean;
   -- True if any queue is non-empty.

   procedure Clear_All;
   -- Drains all three queues.

end Display_Message_Queue;
