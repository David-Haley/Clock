-- Simulation stub for Linux_Signals.
-- Signal handling is not needed in the simulator (docker stop handles
-- teardown). Install and Remove are no-ops; Signal_Stop always returns False.
-- This avoids a GNAT deadlock that occurs when Ada.Interrupts.Attach_Handler
-- is called from inside a protected body (the interrupt manager and protected
-- object mutexes form a lock-ordering cycle on this GNAT/Linux version).

package body Linux_Signals is

   Ctrl_C_Received : Boolean := False;

   function Ctrl_C_Stop return Boolean is (Ctrl_C_Received);

   protected body Handlers is

      procedure Install is
      begin
         null;
      end Install;

      procedure SIGTERM_Handler is
      begin
         SIGTERM_Received := True;
      end SIGTERM_Handler;

      function Signal_Stop return Boolean is (SIGTERM_Received);

      procedure Remove is
      begin
         null;
      end Remove;

   end Handlers;

end Linux_Signals;
