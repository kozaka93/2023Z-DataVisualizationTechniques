package pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt;

import java.util.ArrayList;
import java.util.List;

public class PokladowyDziennikTechniczny {

	protected String idSzybowca; 
	protected List<Wpis> wpisy = new ArrayList<Wpis>();

	public PokladowyDziennikTechniczny(String idSzybowca) {
		this.idSzybowca = idSzybowca;
	}
	
	public void addWpis(Wpis nowyWpis) {
		wpisy.add(nowyWpis);
	}
	
}
