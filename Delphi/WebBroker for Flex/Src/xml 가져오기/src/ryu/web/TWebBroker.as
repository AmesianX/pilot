package ryu.web
{
	import mx.collections.ArrayCollection;
	import mx.rpc.events.FaultEvent;
	import mx.rpc.events.ResultEvent;
	import mx.rpc.http.HTTPService;
		
	public class TWebBroker
	{
		public function TWebBroker()
		{
			_HttpLoader.addEventListener(ResultEvent.RESULT, on_HttpLoaderResult);
			_HttpLoader.addEventListener(FaultEvent.FAULT, on_HttpLoaderFault);
		}
		
		public function Get(AURL:String):void
		{
			_HttpLoader.url = AURL;
			_HttpLoader.method = "get";
			_HttpLoader.send(null); 
		}
		
		[Bindable] public var DataSource : ArrayCollection;
		
		private var _HttpLoader : HTTPService = new HTTPService();
		
		private function on_HttpLoaderResult(event:ResultEvent):void
		{
			DataSource = event.result.Records.Record;	
		}

		private function on_HttpLoaderFault(event:FaultEvent):void
		{
			DataSource = null;	
		}
	}
}