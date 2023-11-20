package pl.edu.pw.mini.zpoif.task4a;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.Puchacz;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.Szybowiec;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt.PokladowyDziennikTechniczny;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt.WpisSkrocony;

public class Generator {

	public static Szybowiec utworzPuchacza() {
		String id = "SP-4110";
		PokladowyDziennikTechniczny pokladowyDziennikTechniczny = new PokladowyDziennikTechniczny(id);
		pokladowyDziennikTechniczny.addWpis(new WpisSkrocony("Jan", "Pan", 4));
		pokladowyDziennikTechniczny.addWpis(new WpisSkrocony("Jan", "Pan", 3));
		pokladowyDziennikTechniczny.addWpis(new WpisSkrocony("Aneta", "Maczek", 5));
		pokladowyDziennikTechniczny.addWpis(new WpisSkrocony("Zenon", "Kwaczek", 6));
		pokladowyDziennikTechniczny.addWpis(new WpisSkrocony("Matty", "Cash", 4));
		
		return new Puchacz(id, pokladowyDziennikTechniczny);
	}

}
