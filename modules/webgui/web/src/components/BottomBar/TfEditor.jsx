import React, { Component } from 'react';
import Window from '../common/Window/Window';
import Picker from './Picker';
import EditorCanvas from './Editor/EditorCanvas'
import styles from './TfEditor.scss'

class TfEditor extends Component {

  constructor(props) {
    super(props);

    this.state = {
      showTfEditor: false,
      value: [
        [1, 3],
        [2, 5],
        [3, 2],
        [4, 6],
        [4.5, 6],
        [5, 6],
        [5, 0],
      ],
    }
    this.toggleTfEditor = this.toggleTfEditor.bind(this);

  }

  toggleTfEditor() {
    this.setState({ showTfEditor: !this.state.showTfEditor });
  }
  render() {
    const showTfEditor = this.state.showTfEditor;
    return(
      <div className={styles.Wrapper}>
        <Picker onClick={this.toggleTfEditor} className={(showTfEditor ? styles.Active : '')}>
          <h2>TF</h2>
        </Picker>
         { showTfEditor && (
          <Window size={{ width: 900, height: 700 }} closeCallback={this.toggleTfEditor} title="Transfer Function Editor" className={styles.Window}>
            <div className={styles.EditorContainer} >
              <EditorCanvas data={this.state.value} width={900} height={660} color={'blue'}/>
            </div>
          </Window>
        )}
      </div>
    );
  }
}

export default TfEditor;
