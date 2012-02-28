function doRequest(srcId, dstId, url)
{
	var text = byId(srcId).value;
	byId(dstId).value = '';
	
	var r = new XMLHttpRequest();
	r._custom_ = {dstId: dstId};
	r.open('POST', url, true);
	r.onreadystatechange = stdOnReadyStateChange;
	r.send(text);
}

function stdOnReadyStateChange()
{
	if (this.readyState == 4)
	{
		if(this.status == 200)
		{
			byId(this._custom_.dstId).value = this.responseText;
		}
		else
		{
		    var error = 'unknown'
		    try{
		        error = this.statusText;
		    }catch(exception1)
		    {
		        try{
		            error = this.status;
		        }catch(exception2)
		        {}
		    }
		    
			alert('Error:\n\n'+error);
		}
	}
}