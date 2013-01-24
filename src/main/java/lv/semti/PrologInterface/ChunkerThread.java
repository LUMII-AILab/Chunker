/*******************************************************************************
 * Copyright 2008, 2009 Institute of Mathematics and Computer Science, University of Latvia; Author: Pēteris Paikens
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************/
package lv.semti.PrologInterface;

import java.util.Hashtable;

import jpl.Query;
import jpl.Term;
import jpl.Variable;

/**
 * 
 * @author Pēteris Paikens
 * Mēģinājums čunkošanu laist atsevišķā thread, lai var paralēli tam turpināt darboties ar UI
 * TODO - pabeigt 
 */
public class ChunkerThread extends Thread {
	private Term teikums;
	private Term answer = null;
	
	public ChunkerThread(Term teikums) {
		super();
		this.teikums = teikums;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void run() {
	    Query query = new Query("parse_all", new Term[] { new jpl.Integer(10), teikums, new Variable("OUT") });
        Hashtable<String,Term> solution = query.oneSolution();
        answer = solution.get("OUT");
	}

	public Term getAnswer() {
		return answer;
	}	
}
