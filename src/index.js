import './main.css';
import { Main } from './Main.elm';

Main.embed(document.getElementById('root'), {
  client_id: process.env.CLIENT_ID,
  client_secret: process.env.CLIENT_SECRET,
});
