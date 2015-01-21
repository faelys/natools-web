with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Simple_Pages.Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 8);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (18, 13, 3);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (23, 12, 2);

   G : constant array (0 .. 27) of Unsigned_8 :=
     (0, 0, 1, 0, 0, 0, 1, 0, 10, 8, 0, 4, 0, 0, 8, 0, 0, 0, 0, 2, 8, 0, 0,
      0, 3, 4, 5, 11);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 28;
         F2 := (F2 + Natural (T2 (K)) * J) mod 28;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 13;
   end Hash;

end Natools.Static_Maps.Web.Simple_Pages.Commands;
