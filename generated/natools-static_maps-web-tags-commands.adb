with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 1) of Natural :=
     (4, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (22, 22);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (27, 9);

   G : constant array (0 .. 38) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 4, 16, 0, 0, 3, 10, 15, 0, 9, 0, 12,
      0, 18, 0, 0, 0, 7, 14, 0, 15, 1, 6, 1, 0, 0, 15, 2, 2);

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
