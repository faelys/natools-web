with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 1) of Natural :=
     (1, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (35, 8);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (0, 7);

   G : constant array (0 .. 38) of Unsigned_8 :=
     (0, 2, 3, 0, 18, 0, 8, 15, 0, 13, 0, 10, 0, 0, 0, 12, 9, 0, 0, 0, 17,
      10, 4, 0, 0, 5, 0, 0, 16, 0, 0, 0, 0, 2, 0, 0, 11, 16, 4);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 39;
         F2 := (F2 + Natural (T2 (K)) * J) mod 39;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 19;
   end Hash;

end Natools.Static_Maps.Web.Tags.Commands;
