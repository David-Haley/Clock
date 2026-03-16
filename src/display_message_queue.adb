-- Priority message queue for the secondary display.
--
-- Author    : David Haley
-- Created   : 17/03/2026

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

package body Display_Message_Queue is

   package Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Display_Messages);

   package Message_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);

   Normal_Queue   : Message_Queues.Queue;
   High_Queue     : Message_Queues.Queue;
   Critical_Queue : Message_Queues.Queue;

   procedure Enqueue (Msg : in Display_Messages) is

   begin -- Enqueue
      case Msg.Priority is
         when Normal   => Normal_Queue.Enqueue (Msg);
         when High     => High_Queue.Enqueue (Msg);
         when Critical => Critical_Queue.Enqueue (Msg);
      end case; -- Msg.Priority
   end Enqueue;

   procedure Dequeue_Next (Msg   : out Display_Messages;
                           Found : out Boolean) is

   begin -- Dequeue_Next
      if Critical_Queue.Current_Use > 0 then
         Critical_Queue.Dequeue (Msg);
         Found := True;
      elsif High_Queue.Current_Use > 0 then
         High_Queue.Dequeue (Msg);
         Found := True;
      elsif Normal_Queue.Current_Use > 0 then
         Normal_Queue.Dequeue (Msg);
         Found := True;
      else
         Found := False;
      end if; -- Critical_Queue.Current_Use > 0
   end Dequeue_Next;

   function Has_Critical return Boolean is
     (Critical_Queue.Current_Use > 0);

   function Has_High return Boolean is
     (High_Queue.Current_Use > 0);

   function Any_Pending return Boolean is
     (Normal_Queue.Current_Use > 0 or else
      High_Queue.Current_Use > 0 or else
      Critical_Queue.Current_Use > 0);

   procedure Clear_All is

      Dummy : Display_Messages;

   begin -- Clear_All
      while Critical_Queue.Current_Use > 0 loop
         Critical_Queue.Dequeue (Dummy);
      end loop; -- Critical_Queue.Current_Use > 0
      while High_Queue.Current_Use > 0 loop
         High_Queue.Dequeue (Dummy);
      end loop; -- High_Queue.Current_Use > 0
      while Normal_Queue.Current_Use > 0 loop
         Normal_Queue.Dequeue (Dummy);
      end loop; -- Normal_Queue.Current_Use > 0
   end Clear_All;

end Display_Message_Queue;
